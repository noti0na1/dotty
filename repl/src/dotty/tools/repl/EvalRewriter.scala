package dotty.tools
package repl

import scala.collection.mutable

import dotc.ast.untpd
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Flags
import dotc.util.Spans.Span

/** Helpers for the placeholder string the parser-stage rewriter
 *  substitutes into the enclosing-source text where each `eval(...)`
 *  call sits. The marker itself lives on `EvalContext.placeholder`
 *  (publicly, so user-side closures can reference it).
 *
 *  `emit` wraps the body in parentheses so the splice produces a
 *  syntactically valid expression in any position the eval call could
 *  have appeared in.
 */
private[repl] object EvalBodyPlaceholder:
  inline def Marker: String = EvalContext.placeholder
  def emit(body: String): String = s"({ $body })"

/** Parse-stage rewriter that augments each `eval(...)` call with
 *  `Eval.bind("name", name)` (or `Eval.bindVar("name", cell)` for
 *  mutable bindings) for every name introduced by an enclosing lambda,
 *  block, or method scope. The body string itself is *not* parsed: it
 *  stays dynamic and is compiled at runtime by `Eval.evalIsolated`.
 *
 *  For `var` captures the rewriter further wraps the eval call in a
 *  `Block` that creates a `VarCell` per captured var, runs eval against
 *  those cells, then writes each cell's value back to the outer var so
 *  mutation inside the eval body propagates to the caller.
 */
object EvalRewriter:

  /** Rewrite all `eval(...)` calls in `trees`. */
  def rewrite(trees: List[untpd.Tree])(using Context): List[untpd.Tree] =
    val tx = new Transformer
    trees.mapConserve { tree =>
      tx.setTopLevel(tree)
      tx.transform(tree)
    }

  /** Rewrite a code string by parsing it, applying the rewriter (with
   *  `initialScope` seeded so nested eval calls inside the body capture
   *  the outer bindings plus any local val/var the body declares), and
   *  pretty-printing the result. Used by `Eval.evalIsolated` so a body
   *  like `val j = 2; eval("i + j")` has its inner eval rewritten to
   *  receive `j` as a binding.
   *
   *  When `outerEnclosingSource` is non-empty (i.e. we're rewriting an
   *  *outer* eval's body and that outer eval came in with its own
   *  enclosing context), each inner eval call's enclosingSource is
   *  composed: outer's enclosingSource with outer's placeholder slot
   *  filled by the outer body, and the outer body in turn carries an
   *  inner placeholder for the inner eval. This way the inner eval's
   *  verification compile reconstructs the entire original lexical
   *  context (the def, the outer body, and the inner body) and
   *  capture-checks them together.
   */
  def rewriteCode(
      code: String,
      initialScope: Array[(String, Boolean)],
      outerEnclosingSource: String = "",
      outerEnclosingTypeParams: String = ""
  )(using Context): String =
    import dotty.tools.dotc.parsing.Parsers.Parser
    import dotty.tools.dotc.util.SourceFile
    val source = SourceFile.virtual("<eval-body>", code)
    val parser = new Parser(source)
    val tree = parser.block()
    val tx = new Transformer
    val seed = initialScope.iterator.map((n, isVar) => CapturedName(n, isVar)).toList
    tx.pushInitialScope(seed)
    if outerEnclosingSource.nonEmpty then
      tx.setNestedContext(code, outerEnclosingSource)
    if outerEnclosingTypeParams.nonEmpty then
      tx.pushOuterTypeParams(outerEnclosingTypeParams)
    tx.transform(tree).show

  /** A captured local.
   *
   *  Most captures are vals/vars (`defParamClause = None`). The `isVar`
   *  flag tells the bind site to wrap mutable captures in a `VarCell`.
   *
   *  Block-local defs are captured by eta-expansion. The recorded
   *  parameter list lets the bind site synthesise
   *  `(p1, ..., pn) => g(p1, ..., pn)` with the original parameter
   *  type annotations preserved, so the typer can give the lambda a
   *  precise function type.
   *
   *  Generic defs (`defTypeParams.nonEmpty`) eta-expand to a
   *  polymorphic function value: `[T] => (p: T) => g[T](p)`. The
   *  typer assigns this an `[T] => T => R` type, which the
   *  `EvalTypeAnnotate` phase records in the binding's `sourceType`.
   *
   *  Givens (`isGiven`) are routed through the runtime's `using`
   *  clause so the eval body can `summon[T]` against them. Named
   *  givens are captured by name (also reachable as `name`); anonymous
   *  givens carry a synthesised `name` and capture via
   *  `summon[givenSummonTpt]` so the f-scope's implicit search
   *  resolves any dependency chain at capture time.
   */
  private final case class CapturedName(
      name: String,
      isVar: Boolean,
      isGiven: Boolean = false,
      defParamClause: Option[List[untpd.ValDef]] = None,
      defTypeParams: List[untpd.TypeDef] = Nil,
      givenSummonTpt: Option[untpd.Tree] = None,
      // For class-member captures: the value at the bind site is
      // `this.<name>` rather than the bare `<name>`. For the var
      // sync-back path, we also write back via `this.<name>`.
      valueRef: Option[untpd.Tree] = None,
      assignTarget: Option[untpd.Tree] = None
  ):
    def isDef: Boolean = defParamClause.isDefined

  private object Names:
    val EvalResult: String = "__eval_result__"
    val SelfThis: String = "__this__"
    def cell(name: String): String = s"${name}__cell"
    /** Suffix for the "shadow-safe" copy of a class-member binding.
     *  We capture each class member twice: under its bare name (so a
     *  body that writes `eval("v")` reads `this.v`) AND under
     *  `<name>__field` (so a body that writes `this.v` — typically
     *  because a method parameter shadows the bare name — can be
     *  rewritten by the runtime to use the `__field` form, which is
     *  guaranteed not to collide with method params).
     */
    def field(name: String): String = s"${name}__field"
    /** Synthetic name for the binding that captures
     *  `<ClassName>.this`. Used when the body refers to the outer
     *  class's instance via `OuterClass.this` from inside a nested
     *  class.
     */
    def qualifiedThis(className: String): String = s"__this__${className}"

  /** What kind of top-level shape encloses the eval call. The
   *  verification compile wraps `Expression` shapes in a synthetic
   *  `val __unused__: Any = { ... }` so the result type-checks; for
   *  `Definition` shapes (def/val/object/class/import) the source is
   *  already a valid module member and gets dropped in as-is.
   */
  private enum TopKind:
    case Unknown, Definition, Expression

  private class Transformer extends untpd.UntypedTreeMap:
    import untpd.*

    /** Stack of in-scope local bindings, innermost on top. Each frame
     *  records the names a single lambda, block, or method introduces.
     */
    private val scopeStack = mutable.Stack.empty[List[CapturedName]]

    /** Stack of in-scope type parameters, innermost on top. Each frame
     *  is the rendered type-param-clause entries of one enclosing
     *  `DefDef` (e.g. `List("T", "U <: AnyRef")` for
     *  `def f[T, U <: AnyRef](...)`). The runtime puts the union of
     *  the stack into the wrapper's `__run__` signature so the body
     *  can refer to those type names directly: erasure means we
     *  don't need any value at runtime, but the body type-checks.
     */
    private val typeParamStack = mutable.Stack.empty[List[String]]

    /** The currently-active top-level tree (one entry of the REPL line's
     *  parsed `trees` list). Used to compute, for each eval call we
     *  encounter, the source text of the enclosing top-level statement
     *  with the eval call's span replaced by `EvalBodyPlaceholder.Marker`.
     *  The runtime verification pass splices the (now known) eval body
     *  string back into that placeholder and re-typechecks the original
     *  lexical context, which is what catches capture-checking violations
     *  the binding-based wrapper compile can't see.
     */
    private var topLevelStart: Int = -1
    private var topLevelEnd: Int = -1
    private var topLevelSource: dotc.util.SourceFile | Null = null
    private var topLevelKind: TopKind = TopKind.Unknown

    /** Nested-eval composition state. When this transformer is rewriting
     *  the body of an *outer* eval (via `rewriteCode`) and that outer
     *  eval came in with its own enclosingSource, we keep both pieces
     *  here so that for each inner eval call we can compose:
     *
     *    outerEnclosingSource[Marker := ({ outerBody[innerSpan := Marker] })]
     *
     *  giving the inner eval an enclosingSource that splices through to
     *  the *original* top-level statement. The inner verification
     *  compile then sees the def/val that owns the outer eval, the
     *  outer eval's body, and the inner eval's body all in one source
     *  unit — which is what CC needs to reason about the inner body's
     *  captures.
     */
    private var nestedOuterBody: String = ""
    private var nestedOuterEnclosingSource: String = ""

    def setTopLevel(tree: Tree)(using Context): Unit =
      val span = tree.span
      if span.exists then
        topLevelStart = span.start
        topLevelEnd = span.end
        topLevelSource = tree.source
        topLevelKind = classifyTopLevel(tree)
      else
        topLevelStart = -1
        topLevelEnd = -1
        topLevelSource = null
        topLevelKind = TopKind.Unknown

    def setNestedContext(outerBody: String, outerEnclosingSource: String): Unit =
      nestedOuterBody = outerBody
      nestedOuterEnclosingSource = outerEnclosingSource

    private def classifyTopLevel(tree: Tree): TopKind = tree match
      case _: DefDef | _: ValDef | _: TypeDef | _: ModuleDef | _: Import => TopKind.Definition
      case _: PackageDef => TopKind.Definition
      case _ => TopKind.Expression

    /** Counter for synthesising names for anonymous given captures.
     *  These names are only used as the wrapper's parameter name; user
     *  code never references them by name (it uses `summon[T]`).
     */
    private var givenCounterValue: Int = 0
    private def freshGivenName(): String =
      val idx = givenCounterValue
      givenCounterValue += 1
      s"__given_$idx"

    /** Seed the scope from outside. Used by `rewriteCode` so nested
     *  eval calls inside a body see the outer bindings.
     */
    def pushInitialScope(names: List[CapturedName]): Unit =
      if names.nonEmpty then scopeStack.push(names)

    /** Seed the type-param stack with the outer eval's enclosing
     *  type-param clause (e.g. `"[T, U <: AnyRef]"`). Used by
     *  `rewriteCode` so a nested eval inherits the outer DefDef's
     *  type params and can reference them in its own wrapper
     *  signature. The clause string is parsed back into entries by
     *  splitting on top-level commas inside the brackets.
     */
    def pushOuterTypeParams(clause: String): Unit =
      val trimmed = clause.trim
      if trimmed.startsWith("[") && trimmed.endsWith("]") then
        val inner = trimmed.substring(1, trimmed.length - 1)
        val entries = splitTopLevel(inner)
        if entries.nonEmpty then typeParamStack.push(entries)

    /** Split a string on top-level commas, ignoring commas inside
     *  brackets / parentheses (so `T <: List[Int, String]` stays as
     *  one entry).
     */
    private def splitTopLevel(s: String): List[String] =
      val out = mutable.ListBuffer.empty[String]
      val sb = new StringBuilder
      var depth = 0
      for c <- s do
        if (c == ',' || c == ';') && depth == 0 then
          val piece = sb.toString.trim
          if piece.nonEmpty then out += piece
          sb.clear()
        else
          if c == '[' || c == '(' || c == '{' then depth += 1
          else if c == ']' || c == ')' || c == '}' then depth -= 1
          sb += c
      val tail = sb.toString.trim
      if tail.nonEmpty then out += tail
      out.toList

    /** Names visible at the current point, deduplicated with innermost
     *  shadowing outer.
     */
    private def currentBindings: List[CapturedName] =
      val seen = mutable.LinkedHashMap.empty[String, CapturedName]
      for level <- scopeStack.iterator; c <- level if !seen.contains(c.name) do
        seen(c.name) = c
      seen.values.toList

    private def withScope[T](names: List[CapturedName])(action: => T): T =
      scopeStack.push(names)
      try action
      finally scopeStack.pop()

    private def withTypeParams[T](typeParams: List[String])(action: => T): T =
      typeParamStack.push(typeParams)
      try action
      finally typeParamStack.pop()

    /** Render the current enclosing type-param environment as a
     *  Scala source-level type-param clause (`"[T, U <: AnyRef]"`),
     *  or the empty string when there are no type params in scope.
     *  Outer-method type params come first; if names collide the
     *  innermost wins (it shadows the outer).
     */
    private def currentTypeParamsString: String =
      val seen = mutable.LinkedHashMap.empty[String, String]
      // typeParamStack is innermost-first; iterate outer-to-inner so
      // the innermost wins on shadowing.
      for level <- typeParamStack.toList.reverse; rendered <- level do
        val name = rendered.takeWhile(c => c.isLetterOrDigit || c == '_' || c == '$')
        seen(name) = rendered
      if seen.isEmpty then ""
      else seen.values.mkString("[", ", ", "]")

    /** Walk a for-comprehension's enumerator list, accumulating
     *  identifier names from each generator/alias pattern into a
     *  cumulative scope. Returns the transformed enumerators and
     *  the cumulative list of names for the body's scope.
     */
    private def transformForEnums(enums: List[Tree])(using Context): (List[Tree], List[CapturedName]) =
      val acc = mutable.ListBuffer.empty[CapturedName]
      val out = enums.map {
        case g @ GenFrom(pat, expr, mode) =>
          val newExpr = withScope(acc.toList)(transform(expr))
          extractPatNames(pat).foreach { name =>
            acc += CapturedName(name, isVar = false)
          }
          untpd.cpy.GenFrom(g)(pat, newExpr, mode)
        case g @ GenAlias(pat, expr) =>
          val newExpr = withScope(acc.toList)(transform(expr))
          extractPatNames(pat).foreach { name =>
            acc += CapturedName(name, isVar = false)
          }
          untpd.cpy.GenAlias(g)(pat, newExpr)
        case other =>
          // `if guard` filter clauses arrive as bare Tree expressions.
          // Transform with the scope accumulated so far.
          withScope(acc.toList)(transform(other))
      }
      (out, acc.toList)

    /** Extract the identifier names a for-comprehension pattern binds.
     *  Currently only handles `Ident` patterns (`for x <- xs`); other
     *  shapes (tuple destructuring, case patterns) return Nil and
     *  silently miss being captured into the body's eval bindings.
     */
    private def extractPatNames(pat: Tree)(using Context): List[String] = pat match
      case Ident(name) if name.toString.nonEmpty && name.toString != "_" =>
        List(name.toString)
      case _ => Nil

    /** The class's type parameters, harvested from the primary
     *  constructor's first paramlist when it's a TypeDef list.
     */
    private def extractClassTypeParams(tmpl: untpd.Template)(using Context): List[String] =
      tmpl.constr.paramss.collectFirst {
        case clause if clause.headOption.exists(_.isInstanceOf[TypeDef]) =>
          clause.collect { case td: TypeDef => renderTypeParam(td) }
      }.getOrElse(Nil)

    /** The class's term-level members visible to an eval call inside
     *  one of its methods. Two sources:
     *
     *    - Constructor parameters with `val` or `var` (which become
     *      class fields).
     *    - `val`/`var` definitions in the class body.
     *
     *  Each member is captured as a binding under its bare name, with
     *  `this.<name>` as the value the rewriter splices at the bind
     *  site. For `var` members the var-cell sync-back path also
     *  writes back via `this.<name>`. We deliberately skip class
     *  defs for now (they'd need eta-expansion that closes over
     *  `this`, which is more involved) and skip private members
     *  (the eval driver compiles in a separate module so it can't
     *  access them).
     *
     *  Also adds `__this__` as a binding bound to plain `this`. The
     *  runtime body-rewrite step rewrites `this.x` in the eval body
     *  to `__this__.x` so the body can use the natural form even
     *  when a method parameter shadows the bare member name.
     */
    private def extractClassMembers(tmpl: untpd.Template, className: String)(using Context): List[CapturedName] =
      val out = mutable.ListBuffer.empty[CapturedName]

      // Use qualified `<ClassName>.this.<name>` for the bind value so
      // that an eval call inside a *nested* class can still capture
      // members of the outer class: bare `this` from inside the
      // nested class would refer to the nested instance, not the
      // outer. Qualified-`this` always disambiguates.
      val classTypeIdent = Ident(className.toTypeName)
      def thisRef(span: Span): untpd.Tree =
        This(classTypeIdent).withSpan(span)
      def thisDot(name: String, span: Span): untpd.Tree =
        Select(thisRef(span), name.toTermName).withSpan(span)

      def addMember(name: String, isVar: Boolean, span: Span): Unit =
        if name.nonEmpty then
          val target = thisDot(name, span)
          // Bare-name binding: read-only access. Shadowed by an
          // inner method-param of the same name (innermost wins in
          // `currentBindings`).
          out += CapturedName(
            name = name,
            isVar = false,
            valueRef = Some(target)
          )
          // Shadow-safe `__field` binding: never collides with user
          // identifiers. For var members it carries the cell-based
          // sync-back so writes through the body's `this.<name>`
          // (rewritten to `<name>__field`) propagate to the outer
          // class instance.
          out += CapturedName(
            name = Names.field(name),
            isVar = isVar,
            valueRef = Some(target),
            assignTarget = if isVar then Some(target) else None
          )

      // Constructor val/var parameters become class fields. We
      // include private fields too because the bind site executes
      // inside one of the class's methods, where private members
      // are accessible. The bind reads `this.v` and passes the
      // *value* across the eval boundary; the wrapper doesn't
      // access the field reflectively so visibility checks happen
      // at the bind site, not the wrapper.
      tmpl.constr.paramss.foreach {
        case clause if clause.headOption.forall(_.isInstanceOf[ValDef]) =>
          clause.foreach {
            case vd: ValDef =>
              val flags = vd.mods.flags
              val isField = flags.is(Flags.ParamAccessor) || flags.is(Flags.Param)
              if flags.is(Flags.Mutable) then addMember(vd.name.toString, isVar = true, vd.span)
              else if isField then addMember(vd.name.toString, isVar = false, vd.span)
            case _ =>
          }
        case _ =>
      }

      // Body val/var members.
      tmpl.body.foreach {
        case vd: ValDef =>
          val flags = vd.mods.flags
          addMember(vd.name.toString, isVar = flags.is(Flags.Mutable), vd.span)
        case _ =>
      }

      // Always add __this__ (the innermost-class binding for body
      // rewrites) and __this__<ClassName> (so a nested-class body
      // that writes `OuterClass.this.x` can still resolve to the
      // outer instance after the body rewrite).
      out += CapturedName(name = Names.SelfThis, isVar = false, valueRef = Some(thisRef(tmpl.span)))
      out += CapturedName(
        name = Names.qualifiedThis(className),
        isVar = false,
        valueRef = Some(thisRef(tmpl.span))
      )

      out.toList

    /** Render an untyped `TypeDef` (a type-param entry in a DefDef's
     *  type-param clause) as a Scala source string. Falls back to the
     *  bare name if `show` fails or produces something un-splice-able.
     */
    private def renderTypeParam(td: untpd.TypeDef)(using Context): String =
      val name = td.name.toString
      try
        // Disable colours so the rendered string never carries ANSI
        // escapes; otherwise they'd land in the synthesised
        // `def __run__[<here>]` clause and trip the eval driver's
        // parser ("illegal character '\\u001b'").
        val printCtx = ctx.fresh.setSetting(ctx.settings.color, "never")
        val rendered = td.show(using printCtx)
        if rendered.startsWith("[") then rendered.drop(1).reverse.dropWhile(_ == ']').reverse
        else rendered
      catch case _: Throwable => name

    override def transform(tree: Tree)(using Context): Tree = tree match
      // For-comprehension (`for x <- xs yield expr` or `for x <- xs do
      // body`). The for-comprehension only desugars to lambda calls
      // (`xs.map(x => expr)`) at the typer stage, so at parser stage
      // we still see `ForYield`/`ForDo` with a list of enumerators.
      // Walk the enumerators in order, accumulating their pattern's
      // identifier names onto the scope before transforming the next
      // generator's source / the guard / the body. We only handle
      // simple `Ident` patterns; tuple / case patterns silently fall
      // through (the body's eval call won't see them as bindings).
      case fy @ ForYield(enums, expr) =>
        val (newEnums, accNames) = transformForEnums(enums)
        val newExpr = withScope(accNames)(transform(expr))
        untpd.cpy.ForYield(fy)(newEnums, newExpr)

      case fd @ ForDo(enums, body) =>
        val (newEnums, accNames) = transformForEnums(enums)
        val newBody = withScope(accNames)(transform(body))
        untpd.cpy.ForDo(fd)(newEnums, newBody)

      // Lambda: its parameters become locals visible inside the body.
      // Lambda parameters are always immutable.
      case fn @ Function(args, body) =>
        val names = args.flatMap {
          case vd: ValDef => Some(CapturedName(vd.name.toString, isVar = false))
          case Ident(n)   => Some(CapturedName(n.toString, isVar = false))
          case _ => None
        }
        val newArgs = args.mapConserve(transform)
        val newBody = withScope(names)(transform(body))
        // `Function` is an untpd-only node; use the `untpd.cpy` singleton
        // directly because the base `TreeCopier` we inherit doesn't expose it.
        untpd.cpy.Function(fn)(newArgs, newBody)

      // Block: process stats in order, accumulating names from each
      // val/var/def/given so subsequent stats and the trailing
      // expression see them. Defs are captured by eta-expansion (see
      // `buildEtaExpansion`); only "simple" defs qualify. Givens are
      // routed through the runtime's `using` clause so `summon` works.
      // Anonymous givens are captured via `summon[<tpt>]` since their
      // parser-stage name is empty.
      case bk @ Block(stats, expr) =>
        val processed = mutable.ListBuffer.empty[Tree]
        var blockNames = List.empty[CapturedName]
        for stat <- stats do
          val newStat = withScope(blockNames)(transform(stat))
          processed += newStat
          stat match
            case vd: ValDef =>
              val flags = vd.mods.flags
              val isGiven = flags.is(Flags.Given)
              if isGiven && vd.name.isEmpty then
                // Anonymous given: synthesise a wrapper-only name; the
                // captured value is `summon[<tpt>]` so any dependency
                // chain is resolved at the f-scope.
                val syntheticName = freshGivenName()
                blockNames = CapturedName(
                  syntheticName,
                  isVar = false,
                  isGiven = true,
                  givenSummonTpt = Some(vd.tpt)
                ) :: blockNames
              else if !vd.name.isEmpty then
                val isVar = flags.is(Flags.Mutable)
                blockNames = CapturedName(
                  vd.name.toString,
                  isVar = isVar,
                  isGiven = isGiven
                ) :: blockNames
              // else: empty-named non-given ValDef (shouldn't really
              // happen in source); skip rather than emit a broken Ident("").
            case dd: DefDef if !dd.name.isEmpty && isCaptureableDef(dd) =>
              val (typeParams, valueParams) = splitParamClauses(dd)
              blockNames = CapturedName(
                dd.name.toString,
                isVar = false,
                defParamClause = Some(valueParams),
                defTypeParams = typeParams
              ) :: blockNames
            case _ =>
        val newExpr = withScope(blockNames)(transform(expr))
        cpy.Block(bk)(processed.toList, newExpr)

      // Class / trait / object definition. The Template body's
      // methods are walked with the class's type parameters and
      // val/var members in scope (so an eval call inside a method
      // can refer to them by name and so the wrapper signature
      // carries the class's type-param clause). We also push a
      // synthetic `__this__` binding bound to `this`, and the
      // runtime body-rewrite step rewrites `this.x` references in
      // the eval body to `__this__.x` so users can name fields
      // via the natural `this.` form even when method parameters
      // shadow the bare name.
      case td @ TypeDef(_, tmpl: Template) =>
        val classTypeParams = extractClassTypeParams(tmpl)
        val classMembers = extractClassMembers(tmpl, td.name.toString)
        val newRhs =
          withTypeParams(classTypeParams)(withScope(classMembers)(transform(tmpl)))
        cpy.TypeDef(td)(td.name, newRhs)

      // Method definition: its term parameters are visible in the
      // body, and so are its type parameters (the runtime copies
      // them into the wrapper's `def __run__[...]` signature so a
      // body like `def f[T] = eval("xs.map[T](...)")` can refer to
      // `T`). Erasure means no runtime value is needed for type
      // params; we just need them named in the wrapper's scope.
      case dd: DefDef =>
        val paramNames = dd.paramss.flatMap { clause =>
          clause.collect { case vd: ValDef => CapturedName(vd.name.toString, isVar = false) }
        }
        val typeParams: List[String] = dd.paramss.collectFirst {
          case clause if clause.headOption.exists(_.isInstanceOf[TypeDef]) =>
            clause.collect { case td: TypeDef => renderTypeParam(td) }
        }.getOrElse(Nil)
        val newRhs =
          withTypeParams(typeParams)(withScope(paramNames)(transform(dd.rhs)))
        cpy.DefDef(dd)(dd.name, dd.paramss, dd.tpt, newRhs)

      // The eval call itself: rewrite to the 4-arg form
      //   eval[T](code, scala.Array(bindings), "", enclosingSource)
      //
      // The third arg's empty string is a sentinel for the expected
      // return type; `EvalTypeAnnotate` fills it in with the source-level
      // rendering of the typer's view of `T` so the eval body type-checks
      // against `T` rather than `Any`.
      //
      // The fourth arg is the source text of the enclosing top-level
      // statement (the REPL line's def/val/expr/etc.) with this eval
      // call's span replaced by `EvalBodyPlaceholder.Marker`. The runtime
      // verification pass splices the (now known) eval body back into
      // that placeholder and re-typechecks the whole thing under the
      // original lexical context. That's what catches capture-checking
      // violations the binding-based wrapper compile can't see (e.g.
      // a body capturing an `IO^` parameter inside a `T -> U` lambda).
      // Empty when we can't compute it; in that case the runtime skips
      // the verification pass.
      //
      // We always emit the 4-arg form (even when there are no captures
      // and an empty bindings array) so the post-typer phase has a
      // uniform shape to recognise.
      case app @ Apply(fn, args) if isEvalCall(fn) =>
        val captured = currentBindings
        val newArgs = args.mapConserve(transform)
        val enclosingSrc = computeEnclosingSource(app.span)
        val enclosingTypeParams = currentTypeParamsString
        if captured.exists(_.isVar) then
          buildVarAwareCall(app, fn, newArgs, captured, enclosingSrc, enclosingTypeParams)
        else
          val bindArgs = captured.map(c => buildBind(c, app.span))
          val arrayArg = buildArray(bindArgs, app.span)
          val tpeLit = Literal(Constant("")).withSpan(app.span)
          val srcLit = Literal(Constant(enclosingSrc)).withSpan(app.span)
          val tpsLit = Literal(Constant(enclosingTypeParams)).withSpan(app.span)
          cpy.Apply(app)(fn, newArgs :+ arrayArg :+ tpeLit :+ srcLit :+ tpsLit)

      case _ => super.transform(tree)
    end transform

    /** Build a Block that:
     *    1. creates a `VarCell` per captured var,
     *    2. invokes eval, passing the cells via `Eval.bindVar`,
     *    3. writes each cell's value back to the corresponding outer var,
     *    4. yields the eval result.
     */
    private def buildVarAwareCall(
        app: Apply,
        fn: Tree,
        newArgs: List[Tree],
        captured: List[CapturedName],
        enclosingSrc: String,
        enclosingTypeParams: String
    )(using Context): Tree =
      val span = app.span

      // 1. cell vals: `val name__cell = Eval.VarCell(<source>)`. For
      // a local `var x` the source is the plain `x`; for a class
      // `var v` it's the `this.v` tree the rewriter recorded.
      val cellDefs: List[Tree] = captured.collect { case c if c.isVar =>
        val cellApply = makeFqn("dotty.tools.repl.Eval.VarCell.apply", span)
        val arg = c.valueRef.getOrElse(Ident(c.name.toTermName).withSpan(span))
        ValDef(
          Names.cell(c.name).toTermName,
          TypeTree(),
          Apply(cellApply, arg :: Nil).withSpan(span)
        ).withSpan(span)
      }

      // 2. the eval call (passing bind / bindVar args). 5-arg form:
      // bindings, `expectedType` sentinel (filled in by
      // `EvalTypeAnnotate` from `[T]`), `enclosingSource` for the
      // verification pass, and `enclosingTypeParams` so the wrapper's
      // `def __run__[...]` can refer to outer type names.
      val bindArgs: List[Tree] = captured.map { c =>
        if c.isVar then buildBindVar(c.name, span) else buildBind(c, span)
      }
      val arrayArg = buildArray(bindArgs, span)
      val tpeLit = Literal(Constant("")).withSpan(span)
      val srcLit = Literal(Constant(enclosingSrc)).withSpan(span)
      val tpsLit = Literal(Constant(enclosingTypeParams)).withSpan(span)
      val rebuiltCall = cpy.Apply(app)(fn, newArgs :+ arrayArg :+ tpeLit :+ srcLit :+ tpsLit)
      val resultDef = ValDef(
        Names.EvalResult.toTermName,
        TypeTree(),
        rebuiltCall
      ).withSpan(span)

      // 3. sync-back assignments: `<target> = name__cell.get()`. The
      // target is the local `name` for plain vars, or `this.name` for
      // class var members.
      val syncs: List[Tree] = captured.collect { case c if c.isVar =>
        val cellRef = Ident(Names.cell(c.name).toTermName).withSpan(span)
        val getCall = Apply(Select(cellRef, "get".toTermName).withSpan(span), Nil).withSpan(span)
        val target = c.assignTarget.getOrElse(Ident(c.name.toTermName).withSpan(span))
        Assign(target, getCall).withSpan(span)
      }

      // 4. yield the eval result.
      val finalExpr = Ident(Names.EvalResult.toTermName).withSpan(span)
      Block(cellDefs ++ (resultDef :: syncs), finalExpr).withSpan(span)
    end buildVarAwareCall

    /** Split a def's `paramss` into its single (or empty) type-param
     *  clause and its single (or empty) value-param clause. Assumes
     *  `isCaptureableDef` has already accepted the def, which ensures
     *  there's at most one clause of each kind.
     */
    private def splitParamClauses(dd: DefDef): (List[TypeDef], List[ValDef]) =
      val tps = dd.paramss.collectFirst {
        case clause if clause.headOption.exists(_.isInstanceOf[TypeDef]) =>
          clause.collect { case td: TypeDef => td }
      }.getOrElse(Nil)
      val vps = dd.paramss.collectFirst {
        case clause if clause.headOption.forall(_.isInstanceOf[ValDef]) && clause.nonEmpty =>
          clause.collect { case vd: ValDef => vd }
      }.getOrElse(Nil)
      (tps, vps)

    /** Whether `dd` can be eta-expanded for capture. Conservative:
     *  rejects shapes the bind-site eta-expansion can't reproduce.
     *
     *  Accepted:
     *    - parameterless defs (`def g = 42`)
     *    - single value paramlist (`def g(p: T): R`)
     *    - single type paramlist with simple bounds, optionally
     *      followed by a single value paramlist
     *      (`def g[T](p: T): R`, `def g[T <: AnyRef](p: T): R`)
     *
     *  Rejected:
     *    - multiple paramlists of either kind
     *    - by-name params, varargs, `implicit`/`given`/`erased` mods
     *    - `inline`/`transparent` defs
     *    - higher-kinded type params (rhs is a non-`TypeBoundsTree`)
     *    - context bounds (those become a separate `using` clause,
     *      caught here by the multi-paramlist check)
     */
    private def isCaptureableDef(dd: DefDef)(using Context): Boolean =
      def acceptableValMods(vd: ValDef): Boolean =
        val flags = vd.mods.flags
        !flags.isOneOf(Flags.Implicit | Flags.Given | Flags.Erased)
      def acceptableTpt(tpt: Tree): Boolean = tpt match
        // ByNameTypeTree marks `=> A`; PostfixOp(_, "*") marks varargs.
        case _: ByNameTypeTree => false
        case PostfixOp(_, op) if op.name.toString == "*" => false
        case _ => true
      def acceptableClause(clause: List[ValDef | TypeDef]): Boolean =
        // Determined by the kind of the first element. Empty clause is fine.
        clause match
          case Nil => true
          case (_: TypeDef) :: _ =>
            clause.forall {
              case td: TypeDef => acceptableTypeParam(td)
              case _ => false
            }
          case _ =>
            clause.forall {
              case vd: ValDef => acceptableValMods(vd) && acceptableTpt(vd.tpt)
              case _ => false
            }

      val mods = dd.mods.flags
      if mods.isOneOf(Flags.Inline | Flags.Transparent) then return false

      // At most one type clause and one value clause.
      val typeClauseCount = dd.paramss.count(_.headOption.exists(_.isInstanceOf[TypeDef]))
      val valueClauseCount = dd.paramss.count {
        case Nil => true
        case (_: ValDef) :: _ => true
        case _ => false
      }
      if typeClauseCount > 1 || valueClauseCount > 1 then return false
      dd.paramss.forall(acceptableClause)

    /** Type params we can replicate on a `PolyFunction`: only those
     *  with a plain `TypeBoundsTree` rhs. Higher-kinded params and
     *  context bounds (which the parser desugars away from the
     *  TypeDef rhs) need more elaborate handling we don't support yet.
     */
    private def acceptableTypeParam(td: TypeDef): Boolean =
      td.rhs match
        case _: TypeBoundsTree => true
        case _ => false

    /** The source text of the enclosing top-level statement, with the
     *  span of the eval call we're rewriting replaced by
     *  `EvalBodyPlaceholder.Marker`. The runtime verification pass
     *  splices the eval body back into that placeholder and re-typechecks
     *  the original lexical context to catch capture-checking violations
     *  the wrapper-compile path can't see.
     *
     *  Returns the empty string when we can't form a valid slice (e.g.
     *  the eval call's span fell outside the top-level tree's span,
     *  which can happen for synthetic spans). The runtime treats the
     *  empty string as "skip the verification pass".
     */
    private def computeEnclosingSource(evalSpan: Span)(using Context): String =
      // Nested mode: we're rewriting an outer eval's body (via
      // `rewriteCode`), and the outer eval supplied us its own
      // enclosingSource. Compose so the inner eval inherits the full
      // chain. The body's tree spans are coordinates within
      // `nestedOuterBody`, so we slice that, drop a Marker where the
      // inner eval sits, then plug the result into the outer
      // enclosingSource's Marker slot (wrapped in `({ ... })` so it
      // splices into any expression position the outer eval was in).
      if nestedOuterEnclosingSource.nonEmpty && nestedOuterBody.nonEmpty then
        if !evalSpan.exists then return ""
        val s = evalSpan.start
        val e = evalSpan.end
        if s < 0 || e > nestedOuterBody.length || s > e then return ""
        val outerBodyWithInnerMarker =
          nestedOuterBody.substring(0, s) + EvalBodyPlaceholder.Marker + nestedOuterBody.substring(e)
        return nestedOuterEnclosingSource.replace(
          EvalBodyPlaceholder.Marker,
          EvalBodyPlaceholder.emit(outerBodyWithInnerMarker)
        )

      val sourceFile = topLevelSource
      if topLevelStart < 0 || !evalSpan.exists || sourceFile == null then return ""
      val src = sourceFile.content
      if topLevelEnd > src.length || topLevelStart >= topLevelEnd then return ""
      val relStart = evalSpan.start - topLevelStart
      val relEnd = evalSpan.end - topLevelStart
      val topLen = topLevelEnd - topLevelStart
      if relStart < 0 || relEnd > topLen || relStart > relEnd then return ""
      val topSrc = String.valueOf(src, topLevelStart, topLen)
      val withMarker =
        topSrc.substring(0, relStart) + EvalBodyPlaceholder.Marker + topSrc.substring(relEnd)
      topLevelKind match
        case TopKind.Definition => withMarker
        case TopKind.Expression => s"val __unused__ : Any = { $withMarker }"
        case TopKind.Unknown => ""

    private def isEvalCall(fn: Tree): Boolean = fn match
      case Ident(n) => isEvalName(n.toString)
      case Select(qual, n) => isEvalName(n.toString) && isEvalQualifier(qual)
      // `eval[T](...)` desugars to `Apply(TypeApply(Ident("eval"), ...), ...)`.
      case TypeApply(inner, _) => isEvalCall(inner)
      case _ => false

    private def isEvalName(name: String): Boolean =
      name == "eval" || name == "evalSafe"

    private def isEvalQualifier(t: Tree): Boolean = t match
      case Ident(n) => n.toString == "Eval"
      case Select(_, n) => n.toString == "Eval"
      case _ => false

    /** Emit the 3-arg `Eval.bind(name, value, "")` form. The empty
     *  string is a sentinel: the post-typer phase `EvalTypeAnnotate`
     *  walks these calls and replaces the literal with the typer's
     *  view of `value`'s source-level type. If the binding never
     *  reaches that phase (e.g. nested-eval runtime rewriting), the
     *  runtime falls back to `Class`-walking.
     *
     *  For def captures, the value is an eta-expansion lambda built
     *  from the original def's parameter list. The typer infers a
     *  precise `FunctionN[..., R]` type for the lambda, which the
     *  type-annotation phase then records.
     */
    private def buildBind(c: CapturedName, span: Span)(using Context): Tree =
      val fqn = if c.isGiven then "dotty.tools.repl.Eval.bindGiven"
                else "dotty.tools.repl.Eval.bind"
      val bindFn = makeFqn(fqn, span)
      val nameLit = Literal(Constant(c.name)).withSpan(span)
      val valueRef = c.valueRef match
        case Some(tree) => tree
        case None => c.givenSummonTpt match
          case Some(tpt) => buildSummonOf(tpt, span)
          case None => c.defParamClause match
            case Some(clause) => buildEtaExpansion(c.name, c.defTypeParams, clause, span)
            case None => Ident(c.name.toTermName).withSpan(span)
      val tpeLit = Literal(Constant("")).withSpan(span)
      Apply(bindFn, nameLit :: valueRef :: tpeLit :: Nil).withSpan(span)

    /** Build `scala.Predef.summon[<tpt>]`. Used to capture anonymous
     *  givens whose parser-stage name is empty: rather than reference
     *  them by an empty Ident, we summon them at the call site (which
     *  is in the same scope where the given was declared, so the
     *  typer's implicit search finds it).
     */
    private def buildSummonOf(tpt: Tree, span: Span)(using Context): Tree =
      val summonRef = makeFqn("scala.Predef.summon", span)
      TypeApply(summonRef, tpt :: Nil).withSpan(span)

    /** Build the eta-expansion lambda for a captured def. Three shapes:
     *
     *    - Parameterless (`def g = 42`):
     *        `() => name`             yielding `Function0[R]`.
     *    - Monomorphic value paramlist (`def g(p: T): R`):
     *        `(p: T) => name(p)`      yielding `T => R`.
     *    - Polymorphic (`def g[T](p: T): R`):
     *        `[T] => (p: T) => name[T](p)`  yielding `[T] => T => R`.
     *
     *  Each fresh param carries the `Param` flag (see
     *  `untpd.makeParameter`); without it the typer rejects the ValDef
     *  as a free declaration. Each fresh type param's rhs (its bounds)
     *  is copied from the original so `def g[T <: AnyRef](...)` keeps
     *  its bound on the polymorphic function value.
     */
    private def buildEtaExpansion(
        name: String,
        typeParams: List[TypeDef],
        clause: List[ValDef],
        span: Span
    )(using Context): Tree =
      val nameRef = Ident(name.toTermName).withSpan(span)

      // Either `g` or `g[T1, ..., Tn]` depending on whether the def
      // has type parameters.
      val typedRef =
        if typeParams.isEmpty then nameRef
        else
          val typeRefs: List[Tree] = typeParams.map(td => Ident(td.name).withSpan(span))
          TypeApply(nameRef, typeRefs).withSpan(span)

      // Build the term-level params and the call body. For nullary
      // defs the body is the typed reference itself; we still wrap it
      // in a `Function(Nil, _)` because PolyFunction requires a value
      // paramlist on its inner Function (see `Parsers.makePolyFunction`).
      val (freshValueParams, callBody) =
        if clause.isEmpty then
          (Nil, typedRef)
        else
          val params: List[ValDef] = clause.map { vd =>
            ValDef(vd.name, vd.tpt, EmptyTree)
              .withMods(Modifiers(Flags.Param))
              .withSpan(span)
          }
          val argRefs: List[Tree] = params.map(p => Ident(p.name).withSpan(span))
          (params, Apply(typedRef, argRefs).withSpan(span))

      val termLambda = Function(freshValueParams, callBody).withSpan(span)

      if typeParams.isEmpty then termLambda
      else
        // Polymorphic case: wrap the term lambda in a PolyFunction.
        val freshTypeParams: List[TypeDef] = typeParams.map { td =>
          TypeDef(td.name, td.rhs)
            .withMods(Modifiers(Flags.Param))
            .withSpan(span)
        }
        PolyFunction(freshTypeParams, termLambda).withSpan(span)

    private def buildBindVar(name: String, span: Span)(using Context): Tree =
      val bindFn = makeFqn("dotty.tools.repl.Eval.bindVar", span)
      val nameLit = Literal(Constant(name)).withSpan(span)
      val cellRef = Ident(Names.cell(name).toTermName).withSpan(span)
      val tpeLit = Literal(Constant("")).withSpan(span)
      Apply(bindFn, nameLit :: cellRef :: tpeLit :: Nil).withSpan(span)

    /** Build `scala.Array(elems...)`, or `scala.Array.empty[Eval.Binding]`
     *  when `elems` is empty. The typed-empty form is critical: a bare
     *  `scala.Array()` infers `Array[Nothing]`, which then unifies the
     *  surrounding `eval[T](..., empty, "")` call's `T = Nothing` and
     *  the wrapper compiles with `def __run__: Nothing`, rejecting
     *  every body.
     */
    private def buildArray(elems: List[Tree], span: Span)(using Context): Tree =
      if elems.isEmpty then
        val emptyRef = ReplCompiler.selectFqn("scala.Array.empty", span)
        val bindingType =
          Select(
            ReplCompiler.selectFqn("dotty.tools.repl.Eval", span),
            "Binding".toTypeName
          ).withSpan(span)
        TypeApply(emptyRef, bindingType :: Nil).withSpan(span)
      else
        val arrayApply = ReplCompiler.selectFqn("scala.Array.apply", span)
        Apply(arrayApply, elems).withSpan(span)

    private def makeFqn(fqn: String, span: Span)(using Context): Tree =
      ReplCompiler.selectFqn(fqn, span)
  end Transformer

end EvalRewriter
