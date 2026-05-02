package dotty.tools
package repl

import scala.collection.mutable

import dotc.ast.untpd
import dotc.core.Constants.Constant
import dotc.core.Contexts.*
import dotc.core.Decorators.*
import dotc.core.Flags
import dotc.util.Spans.Span

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
    trees.mapConserve(tx.transform(_))

  /** Rewrite a code string by parsing it, applying the rewriter (with
   *  `initialScope` seeded so nested eval calls inside the body capture
   *  the outer bindings plus any local val/var the body declares), and
   *  pretty-printing the result. Used by `Eval.evalIsolated` so a body
   *  like `val j = 2; eval("i + j")` has its inner eval rewritten to
   *  receive `j` as a binding.
   */
  def rewriteCode(code: String, initialScope: Array[(String, Boolean)])(using Context): String =
    import dotty.tools.dotc.parsing.Parsers.Parser
    import dotty.tools.dotc.util.SourceFile
    val source = SourceFile.virtual("<eval-body>", code)
    val parser = new Parser(source)
    val tree = parser.block()
    val tx = new Transformer
    val seed = initialScope.iterator.map((n, isVar) => CapturedName(n, isVar)).toList
    tx.pushInitialScope(seed)
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
      givenSummonTpt: Option[untpd.Tree] = None
  ):
    def isDef: Boolean = defParamClause.isDefined

  private object Names:
    val EvalResult: String = "__eval_result__"
    def cell(name: String): String = s"${name}__cell"

  private class Transformer extends untpd.UntypedTreeMap:
    import untpd.*

    /** Stack of in-scope local bindings, innermost on top. Each frame
     *  records the names a single lambda, block, or method introduces.
     */
    private val scopeStack = mutable.Stack.empty[List[CapturedName]]

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

    override def transform(tree: Tree)(using Context): Tree = tree match
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

      // Method definition: its term parameters are visible in the body.
      // Method parameters are always immutable in Scala.
      case dd: DefDef =>
        val paramNames = dd.paramss.flatMap { clause =>
          clause.collect { case vd: ValDef => CapturedName(vd.name.toString, isVar = false) }
        }
        val newRhs = withScope(paramNames)(transform(dd.rhs))
        cpy.DefDef(dd)(dd.name, dd.paramss, dd.tpt, newRhs)

      // The eval call itself: splice in an
      // `Array(Eval.bind/bindVar(...), ...)` argument for every captured
      // local. When any captures are vars, also wrap the call in a Block
      // that creates `VarCell`s per var and syncs them back after eval.
      case app @ Apply(fn, args) if isEvalCall(fn) =>
        val captured = currentBindings
        if captured.isEmpty then super.transform(tree)
        else
          val newArgs = args.mapConserve(transform)
          if !captured.exists(_.isVar) then
            val bindArgs = captured.map(c => buildBind(c, app.span))
            cpy.Apply(app)(fn, newArgs :+ buildArray(bindArgs, app.span))
          else
            buildVarAwareCall(app, fn, newArgs, captured)

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
        captured: List[CapturedName]
    )(using Context): Tree =
      val span = app.span

      // 1. cell vals: `val name__cell = Eval.VarCell(name)`.
      val cellDefs: List[Tree] = captured.collect { case c if c.isVar =>
        val cellApply = makeFqn("dotty.tools.repl.Eval.VarCell.apply", span)
        val arg = Ident(c.name.toTermName).withSpan(span)
        ValDef(
          Names.cell(c.name).toTermName,
          TypeTree(),
          Apply(cellApply, arg :: Nil).withSpan(span)
        ).withSpan(span)
      }

      // 2. the eval call (passing bind / bindVar args).
      val bindArgs: List[Tree] = captured.map { c =>
        if c.isVar then buildBindVar(c.name, span) else buildBind(c, span)
      }
      val rebuiltCall = cpy.Apply(app)(fn, newArgs :+ buildArray(bindArgs, span))
      val resultDef = ValDef(
        Names.EvalResult.toTermName,
        TypeTree(),
        rebuiltCall
      ).withSpan(span)

      // 3. sync-back assignments: `name = name__cell.get()`.
      val syncs: List[Tree] = captured.collect { case c if c.isVar =>
        val cellRef = Ident(Names.cell(c.name).toTermName).withSpan(span)
        val getCall = Apply(Select(cellRef, "get".toTermName).withSpan(span), Nil).withSpan(span)
        Assign(Ident(c.name.toTermName).withSpan(span), getCall).withSpan(span)
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

    private def isEvalCall(fn: Tree): Boolean = fn match
      case Ident(n) => n.toString == "eval"
      case Select(qual, n) => n.toString == "eval" && isEvalQualifier(qual)
      // `eval[T](...)` desugars to `Apply(TypeApply(Ident("eval"), ...), ...)`.
      case TypeApply(inner, _) => isEvalCall(inner)
      case _ => false

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
      val valueRef = c.givenSummonTpt match
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

    /** Build `scala.Array(elems...)`. */
    private def buildArray(elems: List[Tree], span: Span)(using Context): Tree =
      val arrayApply = ReplCompiler.selectFqn("scala.Array.apply", span)
      Apply(arrayApply, elems).withSpan(span)

    private def makeFqn(fqn: String, span: Span)(using Context): Tree =
      ReplCompiler.selectFqn(fqn, span)
  end Transformer

end EvalRewriter
