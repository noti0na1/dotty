package dotty.tools
package dotc
package mutability

import core.*
import Symbols.*, Contexts.*, Types.*, ContextOps.*, Decorators.*, SymDenotations.*
import NameKinds.*, Denotations.Denotation
import Names.*, Definitions.*, Symbols.*
import Flags.{Method, JavaDefined}
import NamerOps.linkConstructorParams
import typer.ProtoTypes.*
import typer.RefChecks.checkAllOverrides
import transform.{Recheck, PreRecheck}
import transform.SymUtils.*
import Ordering.Implicits.*
import MutabilityOps.*

import annotation.tailrec
import dotty.tools.dotc.core.Types.ExprType

object CheckMutability:
  import ast.tpd.*

  // Not used right now, but this is required by Recheck
  class Pre extends PreRecheck, DenotTransformers.IdentityDenotTransformer:
    override def isEnabled(using Context) = ctx.settings.Ymut.value
  end Pre

class CheckMutability extends Recheck:
  thisPhase =>

  import ast.tpd.*

  def phaseName: String = "CheckMutability"

  override def isEnabled(using Context) = ctx.settings.Ymut.value

  override def newRechecker()(using Context) = MutabilityChecker(ctx)

  override def run(using Context): Unit =
    checkOverrides.traverse(ctx.compilationUnit.tpdTree)
    super.run

  def checkOverrides = new TreeTraverser:
    def traverse(t: Tree)(using Context) =
      t match
        case t: Template => checkAllOverrides(ctx.owner.asClass)
        case _ =>
      traverseChildren(t)

  class MutabilityChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

    private def isRegularMethod(sym: Symbol)(using Context): Boolean =
      sym.isRealMethod && !sym.isStatic // && !sym.isConstructor

    // private def showMutability(tp: Type)(using Context): String = ???

    /** Substitutes `@polyread` in `tp` with `@mut` */
    private def substPoly(tp: Type, mut: Type)(using Context): Type =
      // assert(mut <:< defn.ReadonlyType)
      // assert(defn.MutableType <:< mut)
      if mut.isRef(defn.PolyreadClass) then tp
      else tp match
        case MutabilityType(tp, mut1) =>
          MutabilityType(tp, mut1.subst(List(defn.PolyreadClass), List(mut)))
        case tp: AnnotatedType =>
          tp.derivedAnnotatedType(substPoly(tp.parent, mut), tp.annot)
        case tp: AndOrType =>
          tp.derivedAndOrType(substPoly(tp.tp1, mut), substPoly(tp.tp2, mut))
        case tp: AppliedType =>
          tp.derivedAppliedType(substPoly(tp.tycon, mut), tp.args.map(substPoly(_, mut)))
        case tp: MethodType =>
          tp.derivedLambdaType(tp.paramNames, tp.paramInfos.map(substPoly(_, mut)), substPoly(tp.resType, mut))
        case tp: ExprType =>
          tp.derivedExprType(substPoly(tp.resType, mut))
        case _ => tp

    /** Find the mutability of receiver from enclosiong method.
     *  For example,
     *  ```
     *  class C:
     *    @readonly def f: Unit =
     *      class D:
     *        def g: C = C.this // error
     *      ...
     *  ```
     *  Here, the mutability of `C.this` is `Readonly`,
     *  which is the mutability of its enclosing method `C.f`.
     */
    def getMutabilityFromEnclosing(tree: Tree)(using Context): Type =
      val cs = tree.symbol

      // Search from the ctx.owner until we find a member
      // whose class symbol is same as the tree.
      @tailrec def search(sym: Symbol): Type =
        if sym == NoSymbol then defn.MutableType
        // find the member of the class
        else if sym.owner == cs then
          // if the member is a method, the mutability of the `this` reference is dependent on the annotation
          if isRegularMethod(sym) then sym.findMutability
          // if the field is lazy or annotated as `mutable`, the mutability should be `Readonly`,
          // since the field should not modify other part of the object.
          else if sym.isField && (sym.is(Flags.Lazy) || sym.hasMutableAnnotation)
          then defn.ReadonlyType
          else defn.MutableType
        else if sym.exists then search(sym.owner)
        else defn.MutableType

      search(ctx.owner)

    override def recheckClassDef(tree: TypeDef, impl: Template, sym: ClassSymbol)(using Context): Type =
      if sym.isReadonlyClass then
        sym.info.parents.foreach {
          case parent: TypeRef =>
            if !(parent.symbol == defn.ObjectClass || parent.symbol.isReadonlyClass) then
              report.error(i"Non-readonly parent $parent is not allowed in readonly class", tree.srcPos)
          case _ =>
        }
      super.recheckClassDef(tree, impl, sym)

    // TODO: check type bounds

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Unit =
      if !sym.is(Flags.Synthetic) && sym.owner.isReadonlyClass then
        if sym.is(Flags.Mutable) then
          report.error(i"A readonly class is not allow to have mutable field $sym", tree.srcPos)
        val mut = sym.info.computeMutability
        if !(mut frozen_=:= defn.ReadonlyType) then
          report.error(i"Field $sym with type ${sym.info} and mutability ${mut} is not allowed in a readonly class", tree.srcPos)
      super.recheckValDef(tree, sym)

    override def recheckThis(tree: This, pt: Type)(using Context): Type =
      val thisTp = super.recheckThis(tree, pt)
      if pt == AnySelectionProto then thisTp
      else MutabilityType(thisTp, getMutabilityFromEnclosing(tree))

    override def recheckSuper(tree: Super, pt: Type)(using Context): Type =
      val superTp = super.recheckSuper(tree, pt)
      if pt == AnySelectionProto then superTp
      else MutabilityType(superTp, getMutabilityFromEnclosing(tree))

    override def recheckSelection(tree: Select, qualType: Type, name: Name,
        sharpen: Denotation => Denotation, isAssign: Boolean)(using Context): Type =
      if name.is(OuterSelectName) then tree.tpe
      else
        //val pre = ta.maybeSkolemizePrefix(qualType, name)
        val mbr = normalizeByName(
          sharpen(
            qualType.findMember(name, qualType,
              excluded = if tree.symbol.is(Flags.Private) then Flags.EmptyFlags else Flags.Private
          )).suchThat(tree.symbol == _))
        val mbrSym = mbr.symbol
        val newType = tree.tpe match
          case prevType: NamedType =>
            val prevDenot = prevType.denot
            val newType = qualType.select(name, mbr)
            if (newType eq prevType) && (mbr.info ne prevDenot.info) && !prevSelDenots.contains(prevType) then
              prevSelDenots(prevType) = prevDenot
            newType
          case _ =>
            qualType.select(name, mbr)
        val selType = constFold(tree, newType)
        val qual = tree.qualifier
        val qualMut = qual match
          case _: This | _: Super => getMutabilityFromEnclosing(qual)
          case _ => qualType.computeMutability

        // Check assign `x.a = ???`,
        // the mutability of `x` must be `Mutable`.
        if isAssign && !(qualMut <:< defn.MutableType) then
          report.error(i"trying to mutate a field on $qualMut $qual", tree.srcPos)

        // Check method selection `x.f(...)`,
        // the mutability of x must be less than or equal to the mutability of `f`.
        if isRegularMethod(mbrSym) then
          val mbrMut = mbrSym.findMutability.asSeenFrom(qualType, mbrSym.owner)
          if mbrMut.isRef(defn.PolyreadClass) then
            return substPoly(selType.widen, qualMut)
          else if !(qualMut <:< mbrMut) && !mbrSym.relaxApplyCheck then
            report.error(i"calling $mbrMut $mbr ${mbrSym.owner} on $qualMut $qual", tree.srcPos)

        // When selecting on a field, the mutability will be at least the mutability of the qualifier.
        // For example, if `p` is readonly, then the type of `p.x` will be `p.x.type @readonly`.
        if mbrSym.isField && !(mbrSym.owner.isReadonlyClass || mbrSym.hasMutableAnnotation) then
          MutabilityType(selType, qualMut)
        else
          selType

    override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
      recheck(tree.fun).widen match
        case fntpe: MethodType =>
          assert(fntpe.paramInfos.hasSameLengthAs(tree.args))
          val formals =
            if tree.symbol.is(JavaDefined) then mapJavaArgs(fntpe.paramInfos)
            else fntpe.paramInfos

          // When multiple parameters have `@polyread`, we compute the max mutability of their arguments,
          // and subsitute the `@polyread` in the result type with the max mutability.
          var polyMut = defn.MutableType
          val relaxedApply = tree.symbol.relaxApplyCheck
          def recheckArgs(args: List[Tree], formals: List[Type], prefs: List[ParamRef]): List[Type] = args match
            case arg :: args1 =>
              var hasPoly = false
              val ftp = formals.head match
                case MutabilityType(tp, fmut) if fmut.isRef(defn.PolyreadClass) =>
                  hasPoly = true
                  MutabilityType(tp, defn.ReadonlyType)
                case tp =>
                  if relaxedApply then
                    MutabilityType(tp, defn.ReadonlyType)
                  else tp

              val argType = recheck(arg, ftp)

              if hasPoly then polyMut = polyMut.union(argType.computeMutability)

              val formals1 =
                if fntpe.isParamDependent
                then formals.tail.map(_.substParam(prefs.head, argType))
                else formals.tail
              argType :: recheckArgs(args1, formals1, prefs.tail)
            case Nil =>
              assert(formals.isEmpty)
              Nil
          val argTypes = recheckArgs(tree.args, formals, fntpe.paramRefs)
          val resTp = constFold(tree, instantiate(fntpe, argTypes, tree.fun.symbol))
          substPoly(resTp, polyMut)
