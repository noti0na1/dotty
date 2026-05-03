package dotty.tools
package repl

import dotc.ast.tpd
import dotc.cc.CaptureAnnotation
import dotc.core.Annotations.Annotation
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Phases.Phase
import dotc.core.Symbols.{NoSymbol, Symbol, defn, requiredModule}
import dotc.core.Types.{AnnotatedType, Type, TypeMap}

/** Post-typer phase that fills in the source-level type of every
 *  `Eval.bind` / `Eval.bindVar` call the parser-stage rewriter produced.
 *
 *  The parser stage emits each call with an empty string as the third
 *  argument: `Eval.bind("name", value, "")`. That sentinel says "the
 *  rewriter doesn't know the type yet". Once the typer has elaborated
 *  the surrounding expression we can read `value.tpe`, render it as a
 *  Scala source string, and substitute the literal so the runtime can
 *  use it directly instead of inferring a type from `value.getClass`.
 *
 *  Concretely this turns:
 *
 *  ```
 *  Eval.bind("g", g, "")
 *  ```
 *
 *  into:
 *
 *  ```
 *  Eval.bind("g", g, "Int => Int")
 *  ```
 *
 *  when `g`'s typer-side type is `Int => Int`. The wrapper module the
 *  runtime synthesises then declares `__run__(`g`: Int => Int, ...)`,
 *  which means `g(2)` inside the eval body type-checks precisely
 *  rather than returning `Any`.
 *
 *  Bindings that don't reach this phase (notably the runtime
 *  nested-eval rewrite, which acts on an untyped tree) keep the empty
 *  sentinel and the runtime falls back to `Class`-walking.
 */
class EvalTypeAnnotate extends Phase:
  import tpd.*

  def phaseName: String = "evalTypeAnnotate"

  protected def run(using Context): Unit =
    val tree = ctx.compilationUnit.tpdTree
    val transformer = new BindAnnotator
    val annotated = transformer.transform(tree)
    if annotated ne tree then ctx.compilationUnit.tpdTree = annotated

  private class BindAnnotator extends TreeMap:
    /** Type-parameter names the rewriter said are in scope at the
     *  current eval call site (extracted from the 5th literal arg).
     *  When non-empty, `renderType` allows these names through even
     *  though they're typer-level type-params, because the runtime
     *  copies them onto the wrapper's `def __run__[...]` signature.
     */
    private var allowedTypeParams: Set[String] = Set.empty

    /** The chain of enclosing scope symbols (DefDef and class
     *  TypeDef) while we descend. Used to validate that an "allowed"
     *  type-param symbol is *actually* owned by some scope enclosing
     *  the eval call — not by a sibling or unrelated definition that
     *  just happens to use the same name. Includes class scopes so
     *  a class type parameter is reachable from a method body inside
     *  that class.
     */
    private val scopeStack = scala.collection.mutable.Stack.empty[Symbol]

    /** True iff `sym` would be shadowed by a closer enclosing scope
     *  having a type parameter with the same name. The rewriter
     *  copies the type-param names of all enclosing scopes into the
     *  wrapper signature with innermost-wins deduplication, so a
     *  shadowed outer T would render as a name that, in the wrapper,
     *  resolves to the *inner* T. We bail in that case to keep the
     *  capture sound at compile time (runtime is fine via erasure).
     */
    private def isShadowedByCloserScope(sym: Symbol)(using Context): Boolean =
      import dotc.core.Flags
      val name = sym.name.toString
      val ownerIdx = scopeStack.toList.indexWhere(_ == sym.maybeOwner)
      if ownerIdx <= 0 then false
      else
        scopeStack.toList.take(ownerIdx).exists { closer =>
          closer.exists && closer.info.decls.exists { d =>
            d.is(Flags.TypeParam) && d.name.toString == name
          }
        }

    override def transform(tree: Tree)(using Context): Tree =
      // Track enclosing DefDef / class TypeDef symbols as we descend
      // so the type-param-owner check can recognise both method type
      // params and class type params, and detect shadowing across
      // nested scopes.
      tree match
        case dd: DefDef =>
          scopeStack.push(dd.symbol)
          try return super.transform(dd) finally scopeStack.pop()
        case td: TypeDef if td.isClassDef =>
          scopeStack.push(td.symbol)
          try return super.transform(td) finally scopeStack.pop()
        case _ =>

      // Handle the binding-side calls first (Eval.bind/bindVar/bindGiven),
      // then the eval call itself. We need both because the eval call
      // wraps the bind calls inside its `Array(...)` argument; we want
      // the inner ones rewritten before the outer one.
      val annotated = tree match
        case app @ Apply(fun, name :: value :: (sentinel @ Literal(Constant(""))) :: Nil)
            if isEvalBindCall(fun) =>
          val isVar = fun.symbol.name.toString == "bindVar"
          // For `bindVar`, the captured value is a `VarCell` (an
          // `AtomicReference[T]`). The runtime evaluator already wraps
          // the recorded source type back into `AtomicReference[...]`
          // when synthesising the wrapper signature, so we want the
          // inner `T`, not the cell type itself.
          val tpe = if isVar then EvalTypeAnnotate.unwrapCellType(value.tpe) else value.tpe
          val tpeStr =
            EvalTypeAnnotate.renderType(tpe, allowedTypeParams, scopeStack.toSet, isShadowedByCloserScope)
          if tpeStr.isEmpty then app
          else
            val tpeLit = Literal(Constant(tpeStr)).withSpan(sentinel.span)
            cpy.Apply(app)(fun, name :: value :: tpeLit :: Nil)

        case app @ Apply(fun, code :: bindings :: (sentinel @ Literal(Constant(""))) :: rest)
            if isEvalCall(fun) =>
          // The rewriter's 5-arg form is
          //   eval[T](code, bindings, "", enclosingSource, enclosingTypeParams)
          // We extract `enclosingTypeParams` (the last literal) so any
          // bind calls *inside* `bindings` can let type-param mentions
          // through when they refer to the wrapper's own type-param
          // clause. We process children with that scope active, then
          // fill the expectedType sentinel from the typed `[T]`.
          val tpsAllow = extractTypeParamNames(rest)
          val previous = allowedTypeParams
          allowedTypeParams = tpsAllow
          val rebuilt = try
            // The rewriter places bind calls inside `bindings`, so we
            // descend into `app` with the type-param scope active.
            // Rendering the eval call's `T` happens *after* this so
            // it also benefits from the allow-list.
            val withChildren = super.transform(app).asInstanceOf[Apply]
            val tArg = extractTypeArg(withChildren.fun)
            val tpeStr =
              if tArg eq null then ""
              else EvalTypeAnnotate.renderType(tArg, allowedTypeParams, scopeStack.toSet, isShadowedByCloserScope)
            withChildren.args match
              case c :: b :: (s @ Literal(Constant(""))) :: r if tpeStr.nonEmpty =>
                val tpeLit = Literal(Constant(tpeStr)).withSpan(s.span)
                cpy.Apply(withChildren)(withChildren.fun, c :: b :: tpeLit :: r)
              case _ => withChildren
          finally allowedTypeParams = previous
          // We've already recursed into children; return rebuilt as-is.
          return rebuilt

        case _ => tree

      super.transform(annotated)

    /** Find the trailing String literal (the rewriter's
     *  `enclosingTypeParams` arg) and parse out the type-param names.
     *  Returns the empty set when the tail isn't a literal we recognise.
     */
    private def extractTypeParamNames(rest: List[Tree]): Set[String] =
      rest.lastOption match
        case Some(Literal(Constant(s: String))) if s.nonEmpty => parseTypeParamNames(s)
        case _ => Set.empty

    private def parseTypeParamNames(clause: String): Set[String] =
      val trimmed = clause.trim
      if !(trimmed.startsWith("[") && trimmed.endsWith("]")) then return Set.empty
      val inner = trimmed.substring(1, trimmed.length - 1)
      // Each entry looks like `T`, `T <: Bound`, `T >: Lo <: Hi`, etc.
      // We only want the leading identifier.
      val out = scala.collection.mutable.Set.empty[String]
      var depth = 0
      val sb = new StringBuilder
      def commit(): Unit =
        val piece = sb.toString.trim
        sb.clear()
        if piece.nonEmpty then
          val name = piece.takeWhile(c => c.isLetterOrDigit || c == '_' || c == '$')
          if name.nonEmpty then out += name
      for c <- inner do
        if c == ',' && depth == 0 then commit()
        else
          if c == '[' || c == '(' || c == '{' then depth += 1
          else if c == ']' || c == ')' || c == '}' then depth -= 1
          sb += c
      commit()
      out.toSet

    private def isEvalBindCall(fun: Tree)(using Context): Boolean =
      val sym = fun.symbol
      sym != NoSymbol
        && EvalNames.BindLike(sym.name.toString)
        && sym.owner == EvalTypeAnnotate.evalModuleClass

    /** Recognise the typed call site of `eval` / `evalSafe` / `agent`
     *  / `agentSafe`. See [[EvalNames]] for the shared name list and
     *  the rationale behind the asymmetric owner check (Eval-owned
     *  for `eval`/`evalSafe`; name-only for `agent`/`agentSafe`).
     */
    private def isEvalCall(fun: Tree)(using Context): Boolean =
      val sym = fun.symbol
      if sym == NoSymbol then return false
      val name = sym.name.toString
      val ownedByEval =
        EvalNames.EvalOwned(name) && sym.owner == EvalTypeAnnotate.evalModuleClass
      ownedByEval || (EvalNames.EvalLike(name) && !EvalNames.EvalOwned(name))

    /** Extract the `T` from a typed `eval[T](...)` `fun` tree. The
     *  parser-stage rewriter never strips the user's `TypeApply`, so
     *  after typer the tree is `TypeApply(Select(Eval, "eval"), tpt)`.
     *  Returns `null` when the tree doesn't carry a type argument
     *  (e.g. someone used the SAM/explicit-overload form), in which
     *  case the rest of the pipeline keeps the empty sentinel.
     */
    private def extractTypeArg(fun: Tree)(using Context): Type | Null = fun match
      case TypeApply(_, tArg :: _) => tArg.tpe
      case _ => null
  end BindAnnotator

end EvalTypeAnnotate

object EvalTypeAnnotate:

  /** Cached lookup of `Eval`'s module class. The phase fires every line
   *  of the REPL session, so we resolve once per Run via the inContext
   *  lookup rather than per-tree.
   */
  private def evalModuleClass(using Context): Symbol =
    requiredModule("dotty.tools.repl.Eval").moduleClass

  /** If `tpe` is `java.util.concurrent.atomic.AtomicReference[X]` (the
   *  underlying type of `Eval.VarCell`), return `X`; otherwise return
   *  `tpe` unchanged. The parser-stage rewriter wraps captured vars
   *  in cells, so the typed `value` argument of every `bindVar` call
   *  reaches us as `AtomicReference[T]`. We peel that wrapper so the
   *  recorded source type is `T`, matching how the runtime synthesises
   *  the wrapper signature.
   */
  private[repl] def unwrapCellType(tpe: Type)(using Context): Type =
    import dotc.core.Types.AppliedType
    if tpe == null || !tpe.exists then return tpe
    tpe.widen match
      case at @ AppliedType(tycon, arg :: Nil)
          if tycon.typeSymbol.fullName.toString == "java.util.concurrent.atomic.AtomicReference" =>
        arg
      case _ => tpe

  /** Render `tpe` as a Scala source string suitable for splicing into
   *  the synthesised eval wrapper. Returns the empty string when the
   *  type is degenerate (NoType, ErrorType, missing) or references a
   *  symbol the eval body wouldn't be able to resolve (an enclosing
   *  method's type parameter, a locally-scoped class, etc.). The
   *  empty result means "leave the binding alone, use the runtime
   *  fallback".
   *
   *  We `widen` to collapse singleton types (a `1` literal has type
   *  `Int(1)`; we want `Int`). We deliberately do *not* dealias: type
   *  aliases the user has in scope (and that the imports the eval
   *  driver injects also bring into scope) are usually preferable to
   *  their expansions, especially for REPL-defined opaque types.
   *
   *  All capture annotations (`^`, `^{...}`) are stripped: the wrapper
   *  module's `__run__` parameters are not the original capabilities,
   *  they're plain values reflectively passed across the eval
   *  classloader boundary, and the cap references inside any
   *  CapturingType wouldn't resolve in the wrapper anyway. The
   *  capture-checking verification pass (which compiles the spliced
   *  body under the *original* lexical context) is what catches CC
   *  violations now; the wrapper compile sees binding types as
   *  untracked.
   */
  private[repl] def renderType(tpe: Type)(using Context): String =
    renderType(tpe, Set.empty, Set.empty, _ => false)

  /** Like the no-arg overload but also allows references to the
   *  type-parameter names in `allowedTypeParams` — but *only* when
   *  the symbol's owner is one of the `enclosingScopes` and a closer
   *  enclosing scope doesn't shadow the same name (`isShadowed`).
   *  This admits both method type params (DefDef-owned) and class
   *  type params (TypeDef-owned), while still rejecting shadowed
   *  outer type params that would silently bind to the inner same-
   *  named one in the wrapper signature.
   */
  private[repl] def renderType(
      tpe: Type,
      allowedTypeParams: Set[String],
      enclosingScopes: Set[Symbol],
      isShadowed: Symbol => Boolean
  )(using Context): String =
    if tpe == null || !tpe.exists || tpe.isError then return ""
    val widened = tpe.widen
    if !widened.exists || widened.isError then return ""
    if isUselessType(widened) then return ""
    val resolved = dealiasLocalAliases(widened)
    if mentionsLocallyScopedSymbol(resolved, allowedTypeParams, enclosingScopes, isShadowed) then return ""
    val cleaned = stripCaptureAnnotations(resolved)
    // Disable colours so the rendered string never contains ANSI
    // escapes that would later confuse the eval driver's parser.
    val printCtx = ctx.fresh.setSetting(ctx.settings.color, "never")
    try
      val rendered = cleaned.show(using printCtx)
      // Path-dependent types that mention `<ClassName>.this.<member>`
      // can't appear in the wrapper signature: the wrapper module
      // isn't lexically inside any of the outer classes. Convert to
      // the projection form `<ClassName>#<member>`, which references
      // the same class without requiring an instance path. Erasure
      // makes the two equivalent at runtime.
      rendered.replace(".this.", "#")
    catch case _: Throwable => ""

  /** Recursively drop `CapturingType` wrappers and `@retains[...]`
   *  annotations from `tpe`. Keeps every other layer (applied types,
   *  refinements, type aliases, etc.) intact so the wrapper signature
   *  matches the caller's intent shape-wise.
   */
  private def stripCaptureAnnotations(tpe: Type)(using Context): Type =
    val mapper = new TypeMap:
      def apply(tp: Type): Type = tp match
        case AnnotatedType(parent, ann) if isCaptureAnnotation(ann) =>
          this(parent)
        case _ =>
          mapOver(tp)
    mapper(tpe)

  private def isCaptureAnnotation(ann: Annotation)(using Context): Boolean =
    ann match
      case _: CaptureAnnotation => true
      case _ =>
        val sym = ann.symbol
        sym.exists && (sym == defn.RetainsAnnot || sym == defn.RetainsCapAnnot)

  /** A type that's not worth pinning into the wrapper signature.
   *
   *  `Nothing` shows up when the typer can't constrain `T` (an
   *  ascription like `val r: Int = eval("1+2")` doesn't propagate
   *  `Int` through overload resolution; T defaults to its lower
   *  bound). Pinning the wrapper's return type to `Nothing` would
   *  reject every body. Falling back to the empty sentinel keeps
   *  the existing call-site cast behaviour for these cases.
   *
   *  `Null` (the bottom of the AnyRef hierarchy under explicit
   *  nulls) would have the same problem.
   */
  private def isUselessType(tpe: Type)(using Context): Boolean =
    val sym = tpe.typeSymbol
    sym.exists && (sym == defn.NothingClass || sym == defn.NullClass)

  /** True iff `tpe` mentions a symbol that wouldn't resolve in the
   *  fresh eval wrapper module. The two cases that matter in practice:
   *
   *    - **Type parameters** of an enclosing method (`def f[T](x: T)`).
   *      `x.tpe` is just `T`, which doesn't exist in the wrapper.
   *    - **Locally-defined classes** (`def f() = { class L; new L }`).
   *      `L`'s symbol is owned by `f`'s term-level scope; the wrapper
   *      can't see into that scope.
   *
   *  Anything REPL-session-level is fine: those symbols live in
   *  `rs$line$N` modules whose contents we already import. So the
   *  check is "does the type mention a symbol whose enclosing module
   *  is a *term*". REPL session symbols are owned by the wrapper
   *  module class, which is not a term, so they pass.
   */
  private def mentionsLocallyScopedSymbol(
      tpe: Type,
      allowedTypeParams: Set[String],
      enclosingScopes: Set[Symbol],
      isShadowed: Symbol => Boolean
  )(using Context): Boolean =
    import dotc.core.Flags
    tpe.existsPart { part =>
      val sym = part.typeSymbol
      sym.exists && {
        val isTypeParam = sym.is(Flags.TypeParam)
        val isTermOwned = sym.maybeOwner.exists && sym.maybeOwner.isTerm
        val nameAllowed =
          allowedTypeParams.nonEmpty && allowedTypeParams.contains(sym.name.toString)
        // The type-param symbol must be owned by some enclosing
        // scope (DefDef or class) of the eval call. Without that
        // check, a same-named type-param in some unrelated location
        // could be mistaken for an in-scope one.
        val ownerOk =
          sym.maybeOwner.exists && enclosingScopes.contains(sym.maybeOwner)
        // ...and must not be shadowed by a closer scope: e.g. inner
        // `def g[T]` shadowing outer `class C[T]`. Erasure makes
        // shadowed bindings still functional at runtime but the
        // wrapper signature would attribute the wrong T.
        val notShadowed = !isShadowed(sym)
        val allowed =
          (isTypeParam || isTermOwned) && nameAllowed && ownerOk && notShadowed
        (isTypeParam || isTermOwned) && !allowed
      }
    }

  /** Dealias type aliases whose symbol is term-owned (i.e. defined
   *  inside a method scope). Session-level aliases live on a module
   *  class and stay aliased — those are nameable in the wrapper via
   *  the imports the runtime injects. Term-owned aliases would not
   *  resolve in the wrapper, so we expand them to the underlying type.
   */
  private def dealiasLocalAliases(tpe: Type)(using Context): Type =
    import dotc.core.Types.{TypeMap, TypeRef}
    val mapper = new TypeMap:
      def apply(tp: Type): Type = tp match
        case ref: TypeRef =>
          val sym = ref.symbol
          if sym.exists && sym.isAliasType && sym.maybeOwner.exists && sym.maybeOwner.isTerm then
            // Expand the alias and continue mapping into the result.
            this(ref.dealias)
          else mapOver(tp)
        case _ => mapOver(tp)
    mapper(tpe)

end EvalTypeAnnotate
