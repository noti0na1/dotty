package dotty.tools
package repl

import scala.util.control.NonFatal

import dotc.ast.untpd
import dotc.Driver
import dotc.classpath.ClassPathFactory
import dotc.core.Contexts.{Context, ContextBase, inContext}
import dotc.core.Decorators.toTermName
import dotc.core.Symbols.defn
import dotc.core.SymbolLoaders
import dotc.reporting.StoreReporter
import dotc.util.ClasspathFromClassloader
import dotc.parsing.Parsers.Parser
import dotc.util.SourceFile
import io.{AbstractFile, AbstractFileClassLoader, ClassPath, VirtualDirectory}

/** Runtime `eval` for the dotty REPL.
 *
 *  `Eval.eval(code)` compiles and runs `code` at runtime, returning the
 *  result as the polymorphic type `T`. The argument can be any `String`.
 *
 *  The REPL parser injects bindings automatically for every lambda
 *  parameter (and block-local val/def) syntactically in scope at the call
 *  site, so `xs.map(z => eval[Int]("z + 1"))` works without the user
 *  writing the bindings explicitly.
 *
 *  The public surface intentionally avoids any Scala-library types
 *  (`Seq`, `ClassTag`, etc.). User wrappers and `Eval` are loaded by
 *  different classloaders, and Scala types resolve to different `Class`
 *  objects across the boundary, causing `LinkageError`. We use only
 *  JVM-intrinsic types (`String`, `Object`, `Array`) on the API surface.
 *
 *  Outside an active REPL session this throws.
 */
object Eval:

  /** A captured binding.
   *
   *  @param name        the source-level name as it appears at the call site.
   *  @param value       the runtime value, or for var captures the
   *                     `VarCell` the eval body mutates and the call site
   *                     reads back from.
   *  @param isVar       true iff this represents a `var` capture.
   *  @param isGiven     true iff this represents a `given` capture. The
   *                     runtime emits given bindings as members of a
   *                     `(using ...)` clause on the synthesised wrapper
   *                     so `summon[T]` inside the eval body resolves
   *                     against them.
   *  @param sourceType  the source-level Scala type the typer inferred
   *                     for this capture, e.g. `"Int"`, `"Int => Int"`,
   *                     `"List[Int]"`. The empty string is the sentinel
   *                     meaning "fall back to runtime `Class`-walking";
   *                     it appears when this binding wasn't annotated
   *                     by `EvalTypeAnnotate` (e.g. nested-eval
   *                     captures, which never reach the typed pipeline).
   */
  final class Binding(
      val name: String,
      val value: Any,
      val isVar: Boolean,
      val isGiven: Boolean,
      val sourceType: String
  ):
    /** Convenience for the common non-given case. */
    def this(name: String, value: Any, isVar: Boolean, sourceType: String) =
      this(name, value, isVar, isGiven = false, sourceType)

    override def toString =
      s"Binding($name, $value, isVar=$isVar, isGiven=$isGiven, sourceType=$sourceType)"

  /** Mutable cell wrapping a captured `var`. We use the JDK's
   *  `AtomicReference` rather than a class of our own: JDK types are
   *  loaded by the bootstrap classloader, so the cell class resolves
   *  to the same `Class` object on both sides of the eval-driver and
   *  REPL classloader boundary. A REPL-defined cell type would resolve
   *  to two distinct `Class` objects (one per loader) and `Method.invoke`
   *  would reject the call with "argument type mismatch".
   */
  type VarCell[T] = java.util.concurrent.atomic.AtomicReference[T]

  object VarCell:
    def apply[T](initial: T): VarCell[T] =
      new java.util.concurrent.atomic.AtomicReference[T](initial)

  /** Capture an immutable binding. The 3-arg form is what the
   *  parser-stage rewriter actually emits; the post-typer phase
   *  `EvalTypeAnnotate` populates `sourceType` with the typer-known
   *  Scala source type. The 2-arg overload is kept for callers who
   *  don't care about the type-annotation pass (notably the nested-eval
   *  rewrite, which runs at runtime against an untyped tree).
   */
  def bind(name: String, value: Any): Binding =
    new Binding(name, value, isVar = false, sourceType = "")

  def bind(name: String, value: Any, sourceType: String): Binding =
    new Binding(name, value, isVar = false, sourceType)

  /** Capture a mutable (`var`) binding via an `AtomicReference`. The
   *  eval body receives the cell, declares a local var initialised from
   *  `cell.get()`, runs, and writes back via `cell.set(...)`. The
   *  call-site rewriter then assigns `cell.get()` to the outer var.
   */
  def bindVar(name: String, cell: VarCell[?]): Binding =
    new Binding(name, cell, isVar = true, sourceType = "")

  def bindVar(name: String, cell: VarCell[?], sourceType: String): Binding =
    new Binding(name, cell, isVar = true, sourceType)

  /** Capture a `given` binding. The runtime evaluator emits these as
   *  members of a `(using ...)` clause on the synthesised wrapper so
   *  `summon[T]` inside the eval body resolves against them. The
   *  binding is also reachable by name when it has one (named givens
   *  like `given x: Int = 7`); anonymous givens use a synthesised
   *  capture name and are only summonable.
   */
  def bindGiven(name: String, value: Any): Binding =
    new Binding(name, value, isVar = false, isGiven = true, sourceType = "")

  def bindGiven(name: String, value: Any, sourceType: String): Binding =
    new Binding(name, value, isVar = false, isGiven = true, sourceType)

  /** Compile-failure descriptor produced by the verify / wrapper
   *  compile and carried back through the `Adapter` boundary.
   *  Surfaced to users as the failure side of [[EvalResult]] (and as
   *  the data behind [[EvalCompileException]] for the throwing
   *  `eval[T]` form).
   *
   *  Why a value type instead of just throwing the exception: making
   *  the failure a value lets `evalSafe` distinguish *its own* compile
   *  error (a `Left(CompileFailure)` from the adapter) from *a
   *  nested eval inside the body throwing at runtime* (a Java
   *  exception that propagates through). Without that split,
   *  `try { ... } catch case e: EvalCompileException => failure(e)`
   *  inside `evalSafe` would silently swallow nested-eval failures.
   *
   *  Lives in `dotty.tools.repl` so the eval-output classloader
   *  shares the `Class` with the REPL infra (see EVAL.md
   *  "Classloader bridging").
   */
  final class CompileFailure(val errors: Array[String], val source: String):
    override def toString: String =
      s"CompileFailure(${errors.length} error(s))"

  /** Adapter installed by the running REPL driver. The `expectedType`
   *  argument is the source-level rendering of the type argument the
   *  caller wrote at the `eval[T](...)` call site. The empty string
   *  means the caller didn't pin a type, in which case the runtime
   *  uses `Any` as the wrapper's return type and relies on the call
   *  site's `asInstanceOf[T]` cast.
   *
   *  `enclosingSource` is the source text of the enclosing top-level
   *  statement at the eval call site, with this eval call's span
   *  replaced by `EvalBodyPlaceholder.Marker`. The runtime splices the
   *  (now known) eval body string back into that placeholder and runs
   *  a separate verification compile of the original lexical context
   *  with capture checking enabled, so violations the wrapper-compile
   *  path can't see (e.g. a body capturing an `IO^` parameter inside
   *  a `T -> U` lambda) are caught. Empty when the rewriter couldn't
   *  compute a slice; the runtime then skips verification.
   *
   *  Returns either the body's value (`Right`) or a [[CompileFailure]]
   *  (`Left`) describing this call's own compile error. Body runtime
   *  exceptions (including a nested eval throwing
   *  [[EvalCompileException]]) propagate through as Java exceptions
   *  rather than being captured here, so callers can distinguish
   *  them from this call's compile state.
   */
  trait Adapter:
    def evalCode(
        code: String,
        bindings: Array[Binding],
        expectedType: String,
        enclosingSource: String,
        enclosingTypeParams: String
    ): Either[CompileFailure, Any]

  private val active = new ThreadLocal[Adapter]

  def withAdapter[T](adapter: Adapter)(thunk: => T): T =
    val prev = active.get
    active.set(adapter)
    try thunk
    finally if prev == null then active.remove() else active.set(prev)

  /** Compile and run `code` against the current REPL session.
   *
   *  Two forms differ by the first argument:
   *
   *    - `eval(code: String, ...)`: the body is the literal/computed
   *      `code` string.
   *    - `eval(gen: EvalContext => String, ...)`: an agent/LLM-style
   *      generator that receives the call-site context (enclosing
   *      source, placeholder marker, captured bindings) and returns
   *      the body string. Scala 3 SAM conversion accepts a function
   *      literal here even though the parameter is
   *      `java.util.function.Function` (JDK type so the API surface
   *      crosses the eval / REPL classloader boundary cleanly — see
   *      EVAL.md "Classloader bridging"; `scala.Function1` would trip
   *      the JVM's loader-constraint check with `LinkageError`).
   *
   *  Defaulted parameters are normally filled in by the parser-stage
   *  rewriter from the call site:
   *
   *    - `bindings`: every term-level name (lambda parameter,
   *      block-local val/var/def/given, method parameter) syntactically
   *      in scope at the call site.
   *    - `expectedType`: the source-level rendering of `T` from the
   *      explicit `eval[T](...)` type argument; empty when not
   *      writable.
   *    - `enclosingSource`: the source of the enclosing top-level
   *      statement with this call's location replaced by
   *      `EvalContext.placeholder`. The runtime splices the body in
   *      and runs a capture-checking verification compile under the
   *      original lexical context, catching CC violations the
   *      wrapper compile can't see.
   *
   *  Direct callers (no rewriter) can leave them at their defaults.
   */
  def eval[T](
      code: String,
      bindings: Array[Binding] = Array.empty[Binding],
      expectedType: String = "",
      enclosingSource: String = "",
      enclosingTypeParams: String = ""
  ): T =
    evalImpl[T](code, bindings, expectedType, enclosingSource, enclosingTypeParams)

  // Closure form: convenience 1-arg overload + the full 5-arg the
  // rewriter emits. We can't put defaults on these because Scala
  // forbids defaults on more than one overload of the same name —
  // the string form already owns them.

  def eval[T](gen: java.util.function.Function[EvalContext, String]): T =
    eval[T](gen, Array.empty[Binding], "", "", "")

  def eval[T](
      gen: java.util.function.Function[EvalContext, String],
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String,
      enclosingTypeParams: String
  ): T =
    val ctx = new EvalContext(enclosingSource, bindings)
    evalImpl[T](gen.apply(ctx), bindings, expectedType, enclosingSource, enclosingTypeParams)

  /** Non-throwing variant of [[eval]]. Returns [[EvalResult]] with
   *  the body's value on success or the [[EvalCompileException]] on a
   *  compile-time failure. Runtime exceptions thrown by the body
   *  itself still propagate (they aren't compile failures). Designed
   *  so an agent can feed `result.error.errors` back into a generator
   *  and retry without wrapping every call in `try`/`catch`.
   *
   *  Same two forms (string body, closure body) and same defaulted
   *  parameters as [[eval]].
   */
  def evalSafe[T](
      code: String,
      bindings: Array[Binding] = Array.empty[Binding],
      expectedType: String = "",
      enclosingSource: String = "",
      enclosingTypeParams: String = ""
  ): EvalResult[T] =
    evalSafeImpl[T](code, bindings, expectedType, enclosingSource, enclosingTypeParams)

  def evalSafe[T](gen: java.util.function.Function[EvalContext, String]): EvalResult[T] =
    evalSafe[T](gen, Array.empty[Binding], "", "", "")

  def evalSafe[T](
      gen: java.util.function.Function[EvalContext, String],
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String,
      enclosingTypeParams: String
  ): EvalResult[T] =
    val ctx = new EvalContext(enclosingSource, bindings)
    evalSafeImpl[T](gen.apply(ctx), bindings, expectedType, enclosingSource, enclosingTypeParams)

  private def evalImpl[T](
      code: String,
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String,
      enclosingTypeParams: String
  ): T =
    evalSafeImpl[T](code, bindings, expectedType, enclosingSource, enclosingTypeParams).get

  private def evalSafeImpl[T](
      code: String,
      bindings: Array[Binding],
      expectedType: String,
      enclosingSource: String,
      enclosingTypeParams: String
  ): EvalResult[T] =
    // Note: we do NOT catch `EvalCompileException` here. Body
    // runtime exceptions — which include a *nested* eval's
    // compile-time failure surfacing as `EvalCompileException` —
    // propagate to the caller. Only this call's own compile error
    // (delivered as `Left(CompileFailure)` from the adapter) becomes
    // an `EvalResult.failure`.
    activeAdapter().evalCode(code, bindings, expectedType, enclosingSource, enclosingTypeParams) match
      case Right(v) => EvalResult.success(v.asInstanceOf[T])
      case Left(f) => EvalResult.failure(f)

  private def activeAdapter(): Adapter =
    val a = active.get
    if a == null then
      throw new IllegalStateException(
        "eval(...) requires an active dotty REPL session"
      )
    a

  /** Cache key for a compiled wrapper. Two calls with the same key
   *  produce bit-identical wrapper bytecode (modulo the UUID class
   *  name), so the second can reuse the first's compiled `__run__`.
   *
   *  The key intentionally includes the resolved `bindingShape` (the
   *  *types* the wrapper signature was rendered with), not just the
   *  binding names: a body like `eval("z + 1")` compiles to a
   *  different wrapper when `z` is `Int` vs. `String`. `enclosingSource`
   *  is in the key so two structurally similar call sites with
   *  different lexical contexts don't collide.
   *
   *  `sessionLoader` scopes entries to the running REPL session.
   *  Two distinct sessions can have identical `replWrapperImports`
   *  values yet wholly different `rs$line$N` classfile contents
   *  (each test in `DynamicEvalTests` is its own session). Comparing
   *  classloaders by reference identity (the default for `AnyRef`
   *  case class fields) is exactly what we want: one cache namespace
   *  per session, no cross-session bleed.
   */
  private case class WrapperKey(
      code: String,
      enclosingSource: String,
      expectedType: String,
      enclosingTypeParams: String,
      bindingShape: String,
      importsKey: String,
      settingsKey: String,
      sessionLoader: ClassLoader
  )

  /** Cached output of a successful wrapper compile. The classloader
   *  is held by strong reference so the wrapper class can't be
   *  unloaded while the entry lives in the cache.
   */
  private final class CompiledWrapper(
      val module: AnyRef,
      val method: java.lang.reflect.Method,
      val classLoader: ClassLoader
  )

  /** Maximum number of distinct call sites we keep wrappers for.
   *  Each entry pins one classloader + one wrapper class, so this
   *  also bounds metaspace growth from caching.
   */
  private val cacheCapacity = 128

  /** Access-order LRU. `LinkedHashMap` with `accessOrder=true` reorders
   *  on every `get`, so all access must be synchronised; the
   *  `synchronizedMap` wrapper handles per-call locking, which is
   *  enough since we only do single `get` and `put` operations.
   */
  private val wrapperCache: java.util.Map[WrapperKey, Either[CompileFailure, CompiledWrapper]] =
    java.util.Collections.synchronizedMap(
      new java.util.LinkedHashMap[WrapperKey, Either[CompileFailure, CompiledWrapper]](16, 0.75f, true) {
        override def removeEldestEntry(
            eldest: java.util.Map.Entry[WrapperKey, Either[CompileFailure, CompiledWrapper]]
        ): Boolean = size() > cacheCapacity
      }
    )

  /** Discard all cached wrappers. Useful for `:reset` and tests.
   *  Cached classloaders become unreachable and eligible for GC.
   */
  def clearCache(): Unit = wrapperCache.clear()

  private def bindingShapeOf(bindings: Array[Binding], bindingTypes: Array[String]): String =
    val sb = new StringBuilder
    var i = 0
    while i < bindings.length do
      if i > 0 then sb.append('|')
      val b = bindings(i)
      sb.append(b.name).append(':').append(bindingTypes(i))
        .append(':').append(b.isVar).append(':').append(b.isGiven)
      i += 1
    sb.toString

  /** Compile `code` against `classLoader`'s classpath using a fresh,
   *  standalone Driver, with each `Binding` exposed as a method parameter
   *  whose declared type is recovered from its runtime `Class`. Loads
   *  and invokes the compiled function with the captured values.
   *
   *  When `replOutDir` is non-null and `replWrapperImports` is non-empty,
   *  the body is also given access to the running REPL session's
   *  user-defined symbols: `replOutDir` is added to the compiler's
   *  classpath so symbols in `rs$line$N` are resolvable, and the import
   *  statements bring those symbols into the body's lexical scope.
   *
   *  Wrapper bytecode is cached by `WrapperKey` so a tight loop
   *  (`xs.map(z => eval("z+1"))`) compiles once and dispatches via
   *  reflection on subsequent iterations. Caching is skipped when
   *  `enclosingSource` is empty: direct callers and runtime-rewritten
   *  nested evals don't have a tight enough discriminator.
   */
  def evalIsolated(
      code: String,
      classLoader: ClassLoader,
      bindings: Array[Binding],
      replOutDir: AbstractFile,
      replWrapperImports: Array[String],
      compilerSettings: Array[String],
      expectedType: String,
      enclosingSource: String = "",
      enclosingTypeParams: String = "",
      evalLogDir: String = ""
  ): Either[CompileFailure, Any] =
    // Per-invocation log: write the enclosing source (with placeholder)
    // and the body the user submitted into `evalLogDir`. The error
    // file is written below if the verify or wrapper compile fails.
    // Timestamp is shared across the three files of a single call.
    val logTimestamp = if evalLogDir.nonEmpty then writeEvalLogStart(evalLogDir, enclosingSource, code) else ""

    // Pre-compute the source-level type name for each binding once.
    // Prefer the typer-supplied `sourceType` (filled in by the
    // `EvalTypeAnnotate` phase). When that's empty, fall back to
    // walking the runtime `Class` of the captured value. For vars the
    // fallback path also pins the cell's inner type for both the
    // parameter signature and the body-local var declaration, so a
    // racing write between the two reads can't desynchronise them.
    //
    // Computed up front because `bindingTypes` is part of the wrapper
    // cache key: two calls with the same source but different captured
    // types compile to different wrappers.
    val bindingTypes: Array[String] = bindings.map { b =>
      if b.sourceType.nonEmpty then b.sourceType
      else if b.isVar then
        val v = b.value.asInstanceOf[VarCell[?]].get()
        if v == null then "Any" else classToTypeName(v.getClass)
      else if b.value == null then "Any"
      else classToTypeName(b.value.getClass)
    }

    // Split bindings into the regular positional clause and a trailing
    // `using` clause for given bindings. Givens move to the using clause
    // so `summon[T]` inside the eval body resolves against them. The
    // wrapper compiles fine with an empty using clause, so we emit one
    // unconditionally when any given is present.
    val plainBindings = bindings.iterator.zipWithIndex.filter(!_._1.isGiven).toArray
    val givenBindings = bindings.iterator.zipWithIndex.filter(_._1.isGiven).toArray

    // Cache lookup. We cache wrapper bytecode keyed on everything that
    // affects compilation output: the body, the lexical context
    // (`enclosingSource`), the type pin, the resolved binding shape,
    // and a fingerprint of the session-level imports + compiler
    // settings. Two calls with the same key produce identical wrappers
    // (modulo the UUID-named module), so the second can reuse the
    // first's compiled `__run__`. The common motivating case is a
    // tight loop like `xs.map(z => eval[Int]("z + 1"))`: same call
    // site, same body, just different `z` values.
    //
    // Skip caching when `enclosingSource` is empty: that's either a
    // direct caller (no rewriter) or a runtime-rewritten nested eval,
    // and we don't have a tight enough discriminator to cache safely.
    val cacheKey: WrapperKey | Null =
      if enclosingSource.isEmpty then null
      else WrapperKey(
        code = code,
        enclosingSource = enclosingSource,
        expectedType = expectedType,
        enclosingTypeParams = enclosingTypeParams,
        bindingShape = bindingShapeOf(bindings, bindingTypes),
        importsKey = replWrapperImports.mkString("\n"),
        settingsKey = compilerSettings.mkString(" "),
        sessionLoader = classLoader
      )

    if cacheKey != null then
      wrapperCache.get(cacheKey) match
        case null =>
          // miss: fall through to the compile path
        case Left(failure) =>
          if logTimestamp.nonEmpty then writeEvalLogError(evalLogDir, logTimestamp, failure)
          return Left(failure)
        case Right(compiled) =>
          return invokeWrapper(compiled, plainBindings, givenBindings)

    val outDir = new VirtualDirectory("<eval-output>")
    val wrapperName = s"__EvalWrapper_${java.util.UUID.randomUUID.toString.replace('-', '_')}"

    // Capture-checking verification pass. Splice the (now known) eval
    // body string into the placeholder the parser-stage rewriter left
    // in `enclosingSource` and compile the resulting source under the
    // original lexical context. CC sees the body's lambdas and captures
    // exactly as if the user had inlined the body by hand, which catches
    // violations the binding-based wrapper compile can't see (a body
    // capturing an `IO^` parameter inside a `T -> U` lambda, for
    // example: the wrapper's `__run__` parameter is just `IO`, with the
    // capture set erased, so CC never gets a chance to flag the inner
    // lambda). Skip when:
    //   - enclosingSource is empty (rewriter couldn't form a slice, or
    //     this is a runtime-rewritten nested eval), or
    //   - capture checking isn't enabled for the session, since the
    //     verification compile re-checks the *whole* enclosing
    //     statement and can surface unrelated errors (e.g. when the
    //     surrounding code uses session-level imports the verify
    //     compile doesn't replay).
    // A failed verification short-circuits the wrapper compile and
    // returns a `Left(CompileFailure)`; the throwing `eval` form
    // converts that to an exception, the non-throwing `evalSafe`
    // form wraps it in `EvalResult.failure`.
    if enclosingSource.nonEmpty && captureCheckingEnabled(compilerSettings) then
      verifyEnclosing(code, enclosingSource, classLoader, replOutDir, replWrapperImports, compilerSettings) match
        case Some(f) =>
          if logTimestamp.nonEmpty then writeEvalLogError(evalLogDir, logTimestamp, f)
          if cacheKey != null then wrapperCache.put(cacheKey, Left(f))
          return Left(f)
        case None =>

    def renderParam(b: Binding, i: Int): String =
      if b.isVar then
        s"`${b.name}__cell`: java.util.concurrent.atomic.AtomicReference[${bindingTypes(i)}]"
      else
        s"`${b.name}`: ${bindingTypes(i)}"

    val plainClause = plainBindings.iterator.map((b, i) => renderParam(b, i)).mkString(", ")
    val givenClause =
      if givenBindings.isEmpty then ""
      else
        "(using " + givenBindings.iterator.map((b, i) => renderParam(b, i)).mkString(", ") + ")"

    val params = s"($plainClause)$givenClause"

    // For var bindings, declare a body-local `var` initialised from the
    // cell, run the body, then write the local back to the cell. This
    // lets the user's body use `name = ...` syntax naturally.
    val varPrelude = bindings.iterator.zipWithIndex.collect {
      case (b, i) if b.isVar =>
        s"  var `${b.name}`: ${bindingTypes(i)} = `${b.name}__cell`.get()"
    }.mkString("\n")

    val varPostlude = bindings.iterator.collect {
      case b if b.isVar =>
        s"    `${b.name}__cell`.set(`${b.name}`)"
    }.mkString("\n")

    // Always import `Eval.{eval, evalSafe}` so the body itself can call
    // either form unqualified — nested evals (and nested evalSafe-based
    // retries) work. The REPL's own ReplCompiler injects the matching
    // import into every user-line wrapper for the same reason.
    val evalImport = "import dotty.tools.repl.Eval.{eval, evalSafe}\n"
    val importBlock =
      if replWrapperImports.isEmpty then evalImport
      else evalImport + replWrapperImports.mkString("", "\n", "\n")

    // Run the rewriter on the user's code so any nested `eval(...)`
    // calls inside the body capture the outer bindings AND any local
    // val/var the body itself declares. This makes a body like
    // `val j = 2; eval("i + j")` work out of the box: the inner eval
    // receives `j` as a binding the same way an outer eval would.
    //
    // We also pass the outer's `enclosingSource` so the rewriter can
    // *compose* an enclosingSource for each inner eval call: outer's
    // enclosingSource with its placeholder filled by the outer body
    // (which itself carries an inner placeholder for the inner eval).
    // This way the inner verification compile reconstructs the full
    // original lexical context — def, outer body, inner body — and
    // capture-checks them as one source.
    val rewrittenCode0 = rewriteUserCode(code, bindings, enclosingSource, enclosingTypeParams, classLoader)
    // For eval calls inside class methods, the rewriter captured a
    // synthetic `__this__` binding pointing at the outer instance.
    // Rewrite `this.<x>` references in the body to `__this__.<x>` so
    // the user's natural `this.` syntax accesses the outer fields
    // instead of the wrapper module's (empty) `this`.
    val rewrittenCode = rewriteThisInBody(rewrittenCode0, bindings, classLoader)
    // True iff *either* rewrite actually changed the body. Used below
    // to gate the `-no-indent` flag for the wrapper compile: the
    // pretty-printer's output isn't safe under indent-significant
    // parsing, but the user's original code might be (intentionally
    // indent-only Scala 3). Comparing by identity / equality is enough
    // because `rewriteUserCode` and `rewriteThisInBody` both early-
    // return the input string when they don't apply.
    val bodyWasRewritten = (rewrittenCode ne code)

    // Pin the wrapper's return type to the caller's `T` when we have it.
    // The body then type-checks against `T` and a mismatch surfaces as a
    // compile error from the eval driver instead of the usual runtime
    // ClassCastException at the call site's `.asInstanceOf[T]`.
    val returnType = if expectedType.isEmpty then "Any" else expectedType

    val bodyBlock =
      if varPrelude.isEmpty && varPostlude.isEmpty then rewrittenCode
      else
        // Ascribe `__eval_result__` as the return type so the body is
        // type-checked against `T` even on the var-sync path.
        s"""$varPrelude
           |  val __eval_result__ : $returnType = {
           |    $rewrittenCode
           |  }
           |$varPostlude
           |  __eval_result__""".stripMargin

    // The rewriter passes the enclosing DefDef's type-param clause
    // here (e.g. `[T, U <: AnyRef]`) so the wrapper can name those
    // types in its signature and body. Erasure means `Method.invoke`
    // doesn't need actual type arguments at runtime; the body just
    // needs `T`/`U` to be in scope to type-check.
    val typeParamClause = enclosingTypeParams

    val source =
      s"""${importBlock}object $wrapperName {
         |  def __run__$typeParamClause$params: $returnType = {
         |    $bodyBlock
         |  }
         |}
         |""".stripMargin

    compileSource(source, classLoader, outDir, replOutDir, compilerSettings, forceNoIndent = bodyWasRewritten) match
      case Left(errs) =>
        val failure = new CompileFailure(errs.toArray, source)
        if logTimestamp.nonEmpty then writeEvalLogError(evalLogDir, logTimestamp, failure)
        if cacheKey != null then wrapperCache.put(cacheKey, Left(failure))
        return Left(failure)
      case Right(()) =>

    // Use a custom classloader for the eval-compiled wrapper that
    // routes `dotty.tools.repl.*` lookups through the AppClassLoader,
    // including JVM-internal `loadClass(name, resolve)` calls during
    // link-time resolution. Without that bridging, `Eval.Binding` would
    // resolve to two different `Class` objects on the eval-output side
    // (URLClassLoader's copy) versus the REPL side (AppClassLoader's
    // copy), and the JVM would reject crossing-the-boundary calls with
    // a `LinkageError`.
    val cl = new EvalOutputClassLoader(outDir, classLoader)
    val cls = cl.loadClass(s"$wrapperName$$")
    val module = cls.getField("MODULE$").get(null)
    val method = cls.getMethods.find(_.getName == "__run__").getOrElse(
      throw new RuntimeException("__run__ method not found in compiled wrapper")
    )
    val compiled = new CompiledWrapper(module, method, cl)
    if cacheKey != null then wrapperCache.put(cacheKey, Right(compiled))
    invokeWrapper(compiled, plainBindings, givenBindings)
  end evalIsolated

  /** Build the positional argument array and invoke the wrapper's
   *  `__run__`. Shared between the cache-miss and cache-hit paths so
   *  argument layout (plain bindings first, then givens, mirroring
   *  the wrapper signature) lives in exactly one place.
   *
   *  Body runtime exceptions (including a *nested* eval throwing
   *  `EvalCompileException`) propagate out as Java exceptions so
   *  callers can distinguish them from this call's own compile
   *  failure. Only the wrapper-compile and verify-compile produce
   *  `Left(CompileFailure)`.
   */
  private def invokeWrapper(
      compiled: CompiledWrapper,
      plainBindings: Array[(Binding, Int)],
      givenBindings: Array[(Binding, Int)]
  ): Either[CompileFailure, Any] =
    val args =
      (plainBindings.iterator.map(_._1.value.asInstanceOf[AnyRef])
        ++ givenBindings.iterator.map(_._1.value.asInstanceOf[AnyRef])).toArray
    try Right(compiled.method.invoke(compiled.module, args*))
    catch case e: java.lang.reflect.InvocationTargetException =>
      // Preserve the user-visible cause; reflection wraps it in an ITE
      // whose `getCause` is normally non-null, but we guard against the
      // pathological case where it isn't.
      val cause = e.getCause
      if cause != null then throw cause else throw e

  /** Write the per-invocation log files for an eval call:
   *
   *    - `eval_<timestamp>_enclosingSource.scala`: the source of the
   *      enclosing top-level statement at the call site, with the
   *      eval call's span replaced by a placeholder. Useful for
   *      replaying the call's lexical context.
   *    - `eval_<timestamp>_code.scala`: the body string the user
   *      submitted to `eval(...)`.
   *
   *  Returns the timestamp string used in the filenames so the
   *  error file (written later, only on compile failure) can share
   *  it. Returns the empty string when logging fails for any reason
   *  — we don't want logging IO errors to fail the eval call itself.
   */
  private def writeEvalLogStart(evalLogDir: String, enclosingSource: String, code: String): String =
    try
      val dir = new java.io.File(evalLogDir)
      if !dir.exists then dir.mkdirs()
      val ts = s"${System.currentTimeMillis}_${java.util.UUID.randomUUID.toString.take(8).replace('-', '_')}"
      val srcFile = new java.io.File(dir, s"eval_${ts}_enclosingSource.scala")
      val codeFile = new java.io.File(dir, s"eval_${ts}_code.scala")
      java.nio.file.Files.writeString(srcFile.toPath, enclosingSource)
      java.nio.file.Files.writeString(codeFile.toPath, code)
      ts
    // Logging is best-effort: an IO error here must not fail the user's
    // eval call. We still surface the cause to stderr so it can be
    // diagnosed (a permissions problem on the log dir, e.g.) rather
    // than silently dropping every entry.
    catch case NonFatal(e) =>
      System.err.println(
        s"[eval-log] WARNING: failed to write log files under '$evalLogDir': " +
        s"${e.getClass.getSimpleName}: ${e.getMessage}")
      ""

  /** Write `eval_<timestamp>_error.scala` carrying the diagnostic
   *  text and the synthesised source the eval driver was trying to
   *  compile. Best-effort: failures are silently swallowed.
   */
  private def writeEvalLogError(evalLogDir: String, timestamp: String, failure: CompileFailure): Unit =
    try
      val dir = new java.io.File(evalLogDir)
      if !dir.exists then dir.mkdirs()
      val errFile = new java.io.File(dir, s"eval_${timestamp}_error.scala")
      val sb = new StringBuilder
      sb ++= "// errors:\n"
      failure.errors.foreach { e =>
        sb ++= "// "
        sb ++= e.replace("\n", "\n// ")
        sb ++= "\n"
      }
      sb ++= "\n// generated source:\n"
      sb ++= failure.source
      java.nio.file.Files.writeString(errFile.toPath, sb.toString)
    catch case NonFatal(e) =>
      System.err.println(
        s"[eval-log] WARNING: failed to write error log under '$evalLogDir': " +
        s"${e.getClass.getSimpleName}: ${e.getMessage}")

  /** Whether the live REPL session has capture checking enabled (via a
   *  `-language:experimental.captureChecking` CLI flag). The
   *  verification pass only runs for CC-enabled sessions, since CC is
   *  what the pass is for and a naive re-compile of the enclosing
   *  statement otherwise risks surfacing unrelated errors (missing
   *  imports, type-mismatch tests that intentionally rely on the
   *  call-site cast, etc.).
   */
  private def captureCheckingEnabled(compilerSettings: Array[String]): Boolean =
    compilerSettings.exists(s => s.contains("captureChecking"))

  /** Splice `code` into the placeholder embedded in `enclosingSource`
   *  (the source of the enclosing top-level statement at the eval call
   *  site) and run a verification compile under the original lexical
   *  context. Returns `Some(CompileFailure)` on compile errors and
   *  `None` on success. The caller (`evalIsolated`) propagates the
   *  failure up as `Left` so the throwing/non-throwing eval forms can
   *  decide what to do with it.
   *
   *  We wrap the spliced source in a synthetic object so the result is
   *  a valid compilation unit. Imports of REPL-session symbols are
   *  prepended so the spliced code resolves session-level names the
   *  same way the wrapper compile does. Capture checking is enabled if
   *  the live REPL was started with the flag (forwarded through
   *  `compilerSettings`).
   */
  private def verifyEnclosing(
      code: String,
      enclosingSource: String,
      classLoader: ClassLoader,
      replOutDir: AbstractFile,
      replWrapperImports: Array[String],
      compilerSettings: Array[String]
  ): Option[CompileFailure] =
    val splicedBody = enclosingSource.replace(EvalBodyPlaceholder.Marker, EvalBodyPlaceholder.emit(code))
    val verifyName = s"__EvalVerify_${java.util.UUID.randomUUID.toString.replace('-', '_')}"
    val evalImport = "import dotty.tools.repl.Eval.eval\n"
    val importBlock =
      if replWrapperImports.isEmpty then evalImport
      else evalImport + replWrapperImports.mkString("", "\n", "\n")
    val source =
      s"""${importBlock}object $verifyName {
         |$splicedBody
         |}
         |""".stripMargin
    val outDir = new VirtualDirectory("<eval-verify>")
    // The verify pass splices the *original* user body source into
    // the original lexical context — no pretty-printer involved — so
    // we keep indent-significant parsing as-is.
    compileSource(source, classLoader, outDir, replOutDir, compilerSettings, forceNoIndent = false) match
      case Left(errs) =>
        Some(new CompileFailure(errs.toArray, source))
      case Right(()) =>
        None
  end verifyEnclosing

  /** Best-effort conversion from a runtime `Class` to a Scala source-level
   *  type name. Generic type info is erased on the JVM, so parameterized
   *  types come back as their raw form (e.g. `List[Int]` becomes `List`).
   *
   *  When the runtime class is an implementation detail whose JVM name
   *  isn't valid Scala source (e.g. `scala.collection.immutable.::` is
   *  encoded as `scala.collection.immutable.$colon$colon`, and lambda
   *  classes have names like `pkg$$Lambda/0x...`), we walk up the
   *  superclass chain to find the first ancestor with a referenceable
   *  name. So `::` becomes `List`, `Map$Map2` becomes `AbstractMap`, etc.
   */
  private def classToTypeName(c: Class[?]): String =
    if c.isArray then s"Array[${classToTypeName(c.getComponentType)}]"
    else c.getName match
      case "boolean"             => "Boolean"
      case "byte"                => "Byte"
      case "short"               => "Short"
      case "char"                => "Char"
      case "int"                 => "Int"
      case "long"                => "Long"
      case "float"               => "Float"
      case "double"              => "Double"
      case "void"                => "Unit"
      case "java.lang.Boolean"   => "Boolean"
      case "java.lang.Byte"      => "Byte"
      case "java.lang.Short"     => "Short"
      case "java.lang.Character" => "Char"
      case "java.lang.Integer"   => "Int"
      case "java.lang.Long"      => "Long"
      case "java.lang.Float"     => "Float"
      case "java.lang.Double"    => "Double"
      case "java.lang.String"    => "String"
      case _ =>
        // REPL-defined classes are nested in a `rs$line$N` wrapper
        // module; their JVM name is `rs$line$N$<rest>`. Translate that
        // back into Scala source form (`rs$line$N.<rest>`) so we don't
        // walk past it to `Object`.
        replWrapperInnerName(c.getName) match
          case Some(name) =>
            val arity = c.getTypeParameters.length
            if arity == 0 then name
            else s"$name[${Array.fill(arity)("?").mkString(", ")}]"
          case None =>
            val target = referenceableClass(c)
            if target == null then "Any"
            else
              val arity = target.getTypeParameters.length
              val n = target.getName
              if arity == 0 then n
              else s"$n[${Array.fill(arity)("?").mkString(", ")}]"

  private def replWrapperInnerName(jvmName: String): Option[String] =
    val prefix = "rs$line$"
    if !jvmName.startsWith(prefix) then return None
    val rest = jvmName.drop(prefix.length)
    val dollarIdx = rest.indexOf('$')
    if dollarIdx <= 0 then return None
    val moduleNum = rest.substring(0, dollarIdx)
    if !moduleNum.forall(_.isDigit) then return None
    val inner = rest.substring(dollarIdx + 1).replace('$', '.')
    if inner.isEmpty then None
    else Some(s"$prefix$moduleNum.$inner")

  /** A class's name is "referenceable" as Scala source if it doesn't
   *  contain any of the special characters the JVM uses to encode names
   *  the source language can't otherwise express ($ for inner classes
   *  and operator-name encoding, / for hidden lambdas, etc.).
   */
  private def isReferenceableName(name: String): Boolean =
    !name.contains('$') && !name.contains('/')

  /** Walk `c`'s superclass chain and (failing that) its declared
   *  interfaces, returning the first ancestor that has a referenceable
   *  name. Returns `null` if nothing matches.
   */
  private def referenceableClass(c: Class[?]): Class[?] | Null =
    if isReferenceableName(c.getName) then return c
    var sup: Class[?] | Null = c.getSuperclass
    while sup != null && !isReferenceableName(sup.getName) do
      sup = sup.getSuperclass
    if sup != null then return sup
    c.getInterfaces.nn.iterator.find(i => isReferenceableName(i.nn.getName)).orNull

  /** Run the parser-stage rewriter over `code` so any nested `eval(...)`
   *  inside it gets bindings injected. The seeded scope contains the
   *  outer bindings (so an inner eval can also see them) plus the
   *  rewriter naturally tracks any val/var declared in the body.
   *
   *  Rewriting goes through dotty's pretty-printer which doesn't
   *  always round-trip cleanly (`match { ... }` may come back as
   *  `match\n {`, which the parser then chokes on). To avoid breaking
   *  bodies that don't actually need rewriting, we only rewrite when
   *  the body looks like it contains a nested `eval(...)` call, and we
   *  fall back to the original code if the rewritten form fails to
   *  re-parse.
   *
   *  The Context for parsing/printing must have its base initialized
   *  before use: `RefinedPrinter` references `defn.orType` while
   *  rendering any `AppliedTypeTree` (`List[T]`, `Array[T]`, etc.) and
   *  that lookup throws an NPE when `ContextBase.initialize()` hasn't
   *  run, which the catch below would silently swallow as a rollback.
   *  Initialising in turn requires a real classpath: we derive it
   *  from the running REPL session's `classLoader` (the same
   *  derivation `EvalDriver` does for the wrapper compile).
   *
   *  Without that initialisation, the very common case of a body
   *  containing a type-annotated def like `def f(x: X): List[Y]`
   *  would lose its rewrite and the inner eval would see an empty
   *  `enclosingSource`.
   */
  private def rewriteUserCode(
      code: String,
      bindings: Array[Binding],
      outerEnclosingSource: String,
      outerEnclosingTypeParams: String,
      classLoader: ClassLoader
  ): String =
    if !mightContainNestedEval(code) then return code
    val ctx = makeRewriteContext(classLoader)
    val initialScope: Array[(String, Boolean)] =
      bindings.map(b => (b.name, b.isVar))
    val rewritten = EvalRewriter.rewriteCode(
      code, initialScope, outerEnclosingSource, outerEnclosingTypeParams
    )(using ctx)
    // The only "expected" failure mode is the pretty-printer's output
    // not surviving a parser round-trip. That falls back to the
    // original code without alarm. Anything else (a thrown exception
    // from parse / transform / show) is a bug we want to surface, so
    // we deliberately don't catch — the silent fallback used to mask
    // a real NPE in the printer for an embarrassingly long time.
    val parseErrors = parseDiagnostics(rewritten)(using ctx)
    if parseErrors.isEmpty then rewritten
    else
      reportRewriteFallback(
        "rewritten body did not re-parse",
        "nested eval calls inside this body will not see captured bindings or chained enclosing source",
        rewritten,
        parseErrors
      )
      code

  /** Build a fully-initialised Context suitable for parsing and
   *  pretty-printing untyped trees inside the runtime rewrite path.
   *  See `rewriteUserCode` for why initialisation is load-bearing.
   *
   *  We force `-no-indent` so the parser treats braces as authoritative
   *  and ignores indentation widths. Dotty's pretty-printer emits
   *  brace-balanced Scala but its leading-whitespace widths are *not*
   *  guaranteed to satisfy the indent-significant parser (the printer
   *  routinely produces a line at 17 spaces when only 16 and 18 are in
   *  the indentation stack, which the parser rejects with "the start of
   *  this line does not match any of the previous indentation widths"
   *  even though the braces match perfectly). Disabling indent rules is
   *  exactly the trade we want: the printer emits braces, the parser
   *  trusts braces. With this set, the round-trip succeeds for bodies
   *  that previously fell back to the unrewritten original.
   */
  private def makeRewriteContext(classLoader: ClassLoader): Context =
    val ctxBase = new ContextBase
    val ctx0 = ctxBase.initialCtx
    val ctx = ctx0.fresh
      .setSetting(ctx0.settings.color, "never")
      .setSetting(ctx0.settings.noindent, true)
    val cp = ClasspathFromClassloader(classLoader)
    val sysCp = Option(System.getProperty("java.class.path")).getOrElse("")
    val sep = java.io.File.pathSeparator
    val combined = Seq(cp, sysCp).filter(_.nonEmpty).mkString(sep)
    ctx.settings.classpath.update(combined)(using ctx)
    ctxBase.initialize()(using ctx)
    ctx

  private def mightContainNestedEval(code: String): Boolean =
    val names = Array("eval", "evalSafe", "agent", "agentSafe")
    names.exists(n => code.contains(s"$n(") || code.contains(s"$n["))

  /** When the bindings include the synthetic `__this__` (i.e. the
   *  rewriter captured the outer-class instance for an eval call
   *  inside a class method), rewrite top-level `this.x` references
   *  in the body:
   *
   *    - `this.x` where `<x>__field` is one of our captured
   *      class-member bindings → rewritten to bare `<x>__field`.
   *      The wrapper has `<x>__field` as a local (val-typed for val
   *      members; cell-backed `var` for var members), guaranteed not
   *      to collide with method-parameter names. Reads see the
   *      bind-site value; writes flow through the var-cell sync-back.
   *    - `this.x` where `<x>__field` is *not* in bindings (e.g. a
   *      class method or a member we didn't capture) → rewritten to
   *      `__this__.x`. The wrapper has `__this__: C[...]` and
   *      accesses the field/method directly. Visibility check
   *      happens at the wrapper compile, so this only succeeds for
   *      accessible members.
   *    - bare `this` (without a `.x` selection) → `__this__`.
   *
   *  Only top-level `this` references are touched: a `this` inside a
   *  body-local class definition refers to *that* class, not the
   *  outer. Falls back to the original code when parsing fails or
   *  the pretty-printer round-trip can't be re-parsed (per EVAL.md
   *  "Pretty-printer round-trip in nested eval"; same caveat).
   */
  private def rewriteThisInBody(code: String, bindings: Array[Binding], classLoader: ClassLoader): String =
    val needsRewrite = bindings.exists(_.name == "__this__")
    if !needsRewrite || !code.contains("this") then return code
    val memberFieldNames: Set[String] =
      bindings.iterator.map(_.name).filter(_.endsWith("__field")).toSet
    // Qualified `__this__<ClassName>` bindings the rewriter pushed
    // for each enclosing class. Used to translate `OuterClass.this`
    // references in the body when the eval is inside a nested class.
    val qualifiedThisNames: Set[String] =
      bindings.iterator
        .map(_.name)
        .filter(n => n.startsWith("__this__") && n != "__this__")
        .toSet
    val ctx = makeRewriteContext(classLoader)
    val source = SourceFile.virtual("<eval-body>", code)
    val parser = new Parser(source)(using ctx)
    val tree = parser.block()
    val mapper = new untpd.UntypedTreeMap:
      var localTemplateDepth = 0
      override def transform(t: untpd.Tree)(using Context): untpd.Tree = t match
        case td: untpd.TypeDef if td.rhs.isInstanceOf[untpd.Template] =>
          localTemplateDepth += 1
          try super.transform(t) finally localTemplateDepth -= 1
        case sel @ untpd.Select(thisTree: untpd.This, name) if localTemplateDepth == 0 =>
          val nameStr = name.toString
          val fieldName = s"${nameStr}__field"
          val qualName = thisTree.qual match
            case ident: untpd.Ident if ident.name.toString.nonEmpty => ident.name.toString
            case _ => ""
          // Prefer the `<x>__field` binding whenever it exists,
          // regardless of whether `this` is qualified. The
          // `__field` binding is cell-backed for var members so
          // writes propagate via sync-back. Routing through
          // `__this__<Class>.x` instead would mutate the outer
          // instance directly, which collides with the cell's
          // sync-back overwriting it. We accept the limitation
          // that name collisions across nested-class members
          // shadow innermost-wins.
          if memberFieldNames.contains(fieldName) then
            untpd.Ident(fieldName.toTermName).withSpan(sel.span)
          else if qualName.nonEmpty && qualifiedThisNames.contains(s"__this__$qualName") then
            untpd.Select(
              untpd.Ident(s"__this__$qualName".toTermName).withSpan(sel.span),
              name.toTermName
            ).withSpan(sel.span)
          else
            // Fall back to the innermost __this__.
            untpd.Select(
              untpd.Ident("__this__".toTermName).withSpan(sel.span),
              name.toTermName
            ).withSpan(sel.span)
        case thisTree: untpd.This if localTemplateDepth == 0 =>
          val qualName = thisTree.qual match
            case ident: untpd.Ident if ident.name.toString.nonEmpty => ident.name.toString
            case _ => ""
          val targetName =
            if qualName.nonEmpty && qualifiedThisNames.contains(s"__this__$qualName") then
              s"__this__$qualName"
            else "__this__"
          untpd.Ident(targetName.toTermName).withSpan(t.span)
        case _ => super.transform(t)
    val rewritten = mapper.transform(tree)(using ctx).show(using ctx)
    // Same policy as `rewriteUserCode`: only swallow the
    // round-trip-fail case; let any thrown exception propagate so it
    // can be diagnosed.
    val parseErrors = parseDiagnostics(rewritten)(using ctx)
    if parseErrors.isEmpty then rewritten
    else
      reportRewriteFallback(
        "rewritten body (this-references) did not re-parse",
        "references to outer-class members from inside the eval body may fail to resolve",
        rewritten,
        parseErrors
      )
      code

  /** Parse `code` under a fresh reporter and return any syntax errors
   *  the parser collected. An empty list means the input parsed clean.
   *  The parser reports syntax errors through the reporter rather than
   *  throwing, so we install a `StoreReporter` and read accumulated
   *  errors out — a real "what's wrong with this" answer rather than a
   *  yes/no the caller can't act on.
   */
  private def parseDiagnostics(code: String)(using outer: Context): Seq[String] =
    val source = dotty.tools.dotc.util.SourceFile.virtual("<verify>", code)
    val storeReporter = new StoreReporter(null)
    val ctx = outer.fresh.setReporter(storeReporter)
    val parser = new dotty.tools.dotc.parsing.Parsers.Parser(source)(using ctx)
    parser.block()
    if storeReporter.hasErrors then
      storeReporter.removeBufferedMessages(using ctx).map(_.message)
    else Nil

  /** Print a warning explaining why the rewriter fell back to the
   *  original code. Includes the parser diagnostics and a numbered
   *  excerpt of the offending source so the caller can match the
   *  error position to a line.
   */
  private def reportRewriteFallback(
      summary: String,
      consequence: String,
      rewritten: String,
      diagnostics: Seq[String]
  ): Unit =
    val sb = new StringBuilder
    sb ++= "[eval-rewrite] WARNING: " ++= summary ++= "; falling back to the original ("
    sb ++= consequence ++= ").\n"
    sb ++= "  parse errors:\n"
    diagnostics.foreach { d =>
      sb ++= "    "
      sb ++= d.replace("\n", "\n    ")
      sb ++= "\n"
    }
    sb ++= "  rewritten source:\n"
    val lines = rewritten.linesIterator.toArray
    val width = lines.length.toString.length
    lines.zipWithIndex.foreach { (line, i) =>
      sb ++= "    "
      sb ++= String.format(s"%${width}d", (i + 1): Integer)
      sb ++= " | "
      sb ++= line
      sb ++= "\n"
    }
    System.err.print(sb.toString)

  /** Classloader for the eval-output VirtualDirectory. Overrides
   *  `loadClass(name, resolve)` (the entry point JVM-internal
   *  link-time class resolution uses) so references to
   *  `dotty.tools.repl.*` from the compiled wrapper are routed
   *  through the *same* classloader the running ReplDriver itself
   *  uses, rather than picked up from the compiler-classpath
   *  URLClassLoader. Otherwise `Eval$Binding`, `Eval$VarCell`, etc.
   *  would resolve to two different `Class` objects on the two
   *  sides of the eval boundary and the JVM would refuse to let the
   *  values cross.
   */
  private final class EvalOutputClassLoader(root: AbstractFile, parent: ClassLoader)
      extends AbstractFileClassLoader(root, parent):

    private val replInfraLoader: ClassLoader =
      classOf[EvalOutputClassLoader].getClassLoader

    override protected def loadClass(name: String, resolve: Boolean): Class[?] =
      val loaded = findLoadedClass(name)
      if loaded != null then
        if resolve then resolveClass(loaded)
        return loaded

      // `dotty.tools.repl.*` always routes through the loader that
      // loaded this class, sharing `Class` objects with the running
      // ReplDriver no matter how the call arrives (user
      // `cl.loadClass(...)` or JVM-internal link-time lookup).
      val c =
        if name.startsWith("dotty.tools.repl.") then
          replInfraLoader.loadClass(name)
        else
          // Child-first: try our own outDir, fall back to parent chain.
          // Going through `getParent` directly avoids re-entering this
          // method via the io.AFCL parent-chain logic.
          try findClass(name)
          catch case _: ClassNotFoundException => getParent.loadClass(name)

      if resolve then resolveClass(c)
      c
  end EvalOutputClassLoader

  /** Compiler used by `evalIsolated` to compile the wrapper module. Mirrors
   *  the relevant slice of `ReplCompiler`'s phase list: we need the standard
   *  frontend plus `EvalTypeAnnotate` so that *nested* `eval[T]` / `agent[T]`
   *  calls inside the body get their expected-type sentinel filled from the
   *  typed `[T]` argument (just like the REPL line itself does). Without
   *  this phase the inner call is compiled with `expectedType = ""`, so the
   *  inner wrapper's return type defaults to `Any`, the inner body is
   *  type-erased to `Any`, and the prompt sent to the LLM omits the type
   *  pin — losing the very signal that lets the model produce a typed
   *  expression.
   */
  private class EvalCompiler extends dotc.Compiler:
    import dotc.core.Phases.Phase
    import dotc.typer.TyperPhase

    // Splice EvalTypeAnnotate immediately after Typer in the standard
    // phase list. Keeping the rest of the list intact preserves error
    // checks (YCheckPositions, etc.) the wrapper compile relies on.
    override protected def frontendPhases: List[List[Phase]] =
      super.frontendPhases.flatMap { group =>
        if group.exists(_.isInstanceOf[TyperPhase]) then
          List(group, List(new EvalTypeAnnotate))
        else List(group)
      }
  end EvalCompiler

  private class EvalDriver extends Driver:
    override def sourcesRequired: Boolean = false
    // Widen visibility so `compileSource` can pre-set the composed
    // classpath on the fresh context before handing it to `setup()`.
    override def initCtx: Context = super.initCtx
  end EvalDriver

  /** Compose the eval driver's `-classpath` from three sources, with a
   *  single update so the settings layer doesn't issue an "Option
   *  -classpath was updated" warning for setting it twice:
   *
   *    - the value of any `-classpath` (or `-cp`) flag in
   *      `compilerSettings` (forwarded from the REPL session),
   *    - the running JVM's classloader chain (so eval can see classes
   *      already loaded in this process — most importantly the scala
   *      stdlib),
   *    - `java.class.path` (a defensive fallback for environments
   *      where the REPL's classloader isn't a URLClassLoader).
   *
   *  Returns the combined classpath value plus `compilerSettings`
   *  with `-classpath`/`-cp` and its argument removed; the caller
   *  passes the combined value to `Settings.classpath.update` and the
   *  remaining args to `Driver.setup`.
   */
  private def composeClasspath(
      compilerSettings: Array[String],
      classLoader: ClassLoader
  ): (String, Array[String]) =
    val cliCp = extractClasspathArg(compilerSettings)
    val cp = ClasspathFromClassloader(classLoader)
    val sysCp = Option(System.getProperty("java.class.path")).getOrElse("")
    val sep = java.io.File.pathSeparator
    val combined = (cliCp.toSeq ++ Seq(cp, sysCp)).filter(_.nonEmpty).mkString(sep)
    val filtered = stripClasspathFlag(compilerSettings)
    (combined, filtered)

  /** Find the first `-classpath`/`-cp` flag in `args` and return its
   *  value (the next element). Returns `None` if no such flag is
   *  present or it has no value.
   */
  private def extractClasspathArg(args: Array[String]): Option[String] =
    val i = args.indexWhere(a => a == "-classpath" || a == "-cp")
    if i < 0 || i + 1 >= args.length then None
    else Some(args(i + 1))

  /** Return `args` with the first `-classpath`/`-cp` flag and its
   *  value removed. Used together with `extractClasspathArg` so we
   *  can rewrite the classpath exactly once via the settings API
   *  without `Driver.setup` then trying to re-set it from CLI args.
   */
  private def stripClasspathFlag(args: Array[String]): Array[String] =
    val i = args.indexWhere(a => a == "-classpath" || a == "-cp")
    if i < 0 then args
    else if i + 1 >= args.length then args.take(i)
    else args.take(i) ++ args.drop(i + 2)

  private def compileSource(
      source: String,
      classLoader: ClassLoader,
      outDir: AbstractFile,
      replOutDir: AbstractFile,
      compilerSettings: Array[String],
      forceNoIndent: Boolean
  ): Either[Seq[String], Unit] =
    val driver = new EvalDriver
    val (classpath, settingsWithoutCp) = composeClasspath(compilerSettings, classLoader)
    val initCtx = driver.initCtx
    initCtx.settings.classpath.update(classpath)(using initCtx)
    driver.setup(settingsWithoutCp, initCtx) match
      case Some((_, ctx0)) =>
        val storeReporter = new StoreReporter(null)
        // The wrapper body comes from the pretty-printer when the
        // rewriter actually applied changes. The printer emits braces
        // correctly but its leading-whitespace widths don't always
        // satisfy the indent-significant parser ("Indentation width of
        // current line N falls between previous widths …"). When
        // that's the case force `-no-indent` so the parser trusts
        // braces; otherwise leave indent-significant parsing on so
        // intentionally indent-only Scala 3 bodies still work.
        val withIndentSetting =
          if forceNoIndent then ctx0.fresh.setSetting(ctx0.settings.noindent, true)
          else ctx0.fresh
        val freshCtx = withIndentSetting
          .setSetting(ctx0.settings.outputDir, outDir)
          .setReporter(storeReporter)
        // Splice the running REPL's output dir onto the compile-time
        // classpath so the body can resolve symbols defined in earlier
        // REPL lines (`rs$line$N`). `platform` requires initialize();
        // `mergeNewEntries` registers the entries in the symbol loader
        // so name resolution actually finds them.
        if replOutDir != null then
          freshCtx.base.initialize()(using freshCtx)
          val replClassPath = ClassPathFactory.newClassPath(replOutDir)(using freshCtx)
          freshCtx.platform.addToClassPath(replClassPath)(using freshCtx)
          SymbolLoaders.mergeNewEntries(
            defn(using freshCtx).RootClass,
            ClassPath.RootPackage,
            replClassPath,
            freshCtx.platform.classPath(using freshCtx)
          )(using freshCtx)
        try
          val compiler = new EvalCompiler
          val run = compiler.newRun(using freshCtx)
          run.compileFromStrings(source :: Nil)
          if storeReporter.hasErrors then
            Left(storeReporter.removeBufferedMessages(using freshCtx).map(_.message))
          else
            Right(())
        catch case NonFatal(e) =>
          Left(Seq(s"Internal compiler error: ${e.getMessage}"))
      case None =>
        Left(Seq("Failed to set up eval driver"))
  end compileSource

end Eval
