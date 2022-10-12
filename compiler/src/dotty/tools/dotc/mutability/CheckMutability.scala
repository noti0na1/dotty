package dotty.tools
package dotc
package mutability

import core._
import Contexts._, Names._, Flags._, Definitions._
import Types._
import Symbols._
import typer.ProtoTypes._
import transform.{Recheck, PreRecheck}
import Decorators._
import MutabilityOps._
import typer.RefChecks.checkAllOverrides
import NamerOps.linkConstructorParams

import annotation.tailrec

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

    override def recheckSelection(tree: Select, qualType: Type, name: Name, pt: Type)(using Context) = {
      val selType = super.recheckSelection(tree, qualType, name, pt)
      val qual = tree.qualifier
      var qualMut = qualType.computeMutability

      qual match
        case qual: This =>
          // when the selection is C.this.x,
          // search the mutability from enclosing method in C

          val cs = qual.symbol

          @tailrec def searchEnclosing(s: Symbol): Symbol =
            if s == NoSymbol || s.isRealMethod && s.owner == cs then s
            else if !s.is(Method) && s.owner == cs then cs.primaryConstructor // TODO
            else if s.exists then searchEnclosing(s.owner)
            else NoSymbol

          val enclosingMethod = searchEnclosing(ctx.owner)

          if enclosingMethod != NoSymbol then
            qualMut = enclosingMethod.findMutability
        case _ =>

      // TODO: handle this type?

      // check assign `x.a = ???`
      // the mutability of x must be Mutable
      if pt == AssignProto then
        // println(i"check assign $tree, $qualType with $preQuli")
        if qualMut > MutabilityQualifier.Mutable then
          report.error(i"trying to mutate a readonly field of $qual", tree.srcPos)

      // check method selection `x.f(...)`
      // the mutability of x must be less than or equal to the mutability of f
      val mbr = qualType.findMember(name, qualType).suchThat(tree.symbol == _)
      val mbrSym = mbr.symbol
      if mbrSym.isRealMethod && !mbrSym.isStatic then
        val mbrMut = mbrSym.findMutability
        if qualMut > mbrMut then
          report.error(i"calling $mbrMut $mbr on $qualMut $qual", tree.srcPos)
        selType.widen match {
          case fntpe: MethodType => fntpe.resType match {
            case AnnotatedType(resType, annot) if annot.symbol == defn.PolyreadAnnot =>
              annot.tree match {
                case Apply(_, List(_: This)) =>
                  // println("new result type: " + MutabilityType(fntpe.resType, qualMut))
                  return fntpe.derivedLambdaType(resType = MutabilityType(resType, qualMut))
                case _ => // TODO: same for default value?
              }
            case _ =>
          }
        }

      // If a field `x` has a polyread annotation at its type (most out),
      // then the mutability of `a.x` is dependent on `a`.
      if !mbrSym.is(Method) then selType.widen match {
        case AnnotatedType(stp, annot)
          if annot.symbol == defn.PolyreadAnnot
            && qualMut != MutabilityQualifier.Mutable =>
          // println("new sel type: " + MutabilityType(selType, qualMut))
          return MutabilityType(selType, qualMut)
        case _ =>
      }

      selType
    }

    override def recheckApply(tree: Apply, pt: Type)(using Context): Type = {
      recheck(tree.fun).widen match
        case fntpe: MethodType =>
          assert(fntpe.paramInfos.hasSameLengthAs(tree.args))
          val formals =
            if tree.symbol.is(JavaDefined) then mapJavaArgs(fntpe.paramInfos)
            else fntpe.paramInfos

          var hasPolyread = false
          var refParam: ParamRef | Null = null
          var refMut: MutabilityQualifier = MutabilityQualifier.Mutable
          fntpe.resType match {
            case AnnotatedType(_, annot) if annot.symbol == defn.PolyreadAnnot =>
              hasPolyread = true
              // TODO: check the polyread annotation's argument
              annot.tree match
                case Apply(_, List(id: Ident)) => id.tpe match
                  case param: ParamRef =>
                    refParam = param
                  case _ =>
                case _ =>
            case _ =>
          }

          def recheckArgs(args: List[Tree], formals: List[Type], prefs: List[ParamRef]): List[Type] = args match
            case arg :: args1 =>
              val argType = recheck(arg, formals.head)
              val pref = prefs.head
              val formals1 =
                if fntpe.isParamDependent
                then formals.tail.map(_.substParam(pref, argType))
                else formals.tail

              // find the parameter and argument refered by polyread annotation
              if hasPolyread && refParam != null && pref == refParam then
                refMut = argType.computeMutability
                refParam = null

              argType :: recheckArgs(args1, formals1, prefs.tail)
            case Nil =>
              assert(formals.isEmpty)
              Nil

          val argTypes = recheckArgs(tree.args, formals, fntpe.paramRefs)
          val appType = constFold(tree, instantiate(fntpe, argTypes, tree.fun.symbol))

          if hasPolyread && refParam != null then
            report.error(i"cannot find the argument for polyread parameter $refParam", tree.srcPos)

          if refMut != MutabilityQualifier.Mutable then
            appType match
              case AnnotatedType(appType, annot) if annot.symbol == defn.PolyreadAnnot =>
                MutabilityType(appType, refMut)
          else appType
            //.showing(i"typed app $tree : $fntpe with ${tree.args}%, % : $argTypes%, % = $result")
    }

    private def insideViewOfPt(sym: Symbol, pt: Type, tree: Tree)(using Context): Type = pt match {
      case AnnotatedType(pt, annot) if annot.symbol == defn.PolyreadAnnot =>
        annot.tree match
          case Apply(_, List(id: Ident)) =>
            id.tpe match
              case param: TermRef =>
                // fields: the owner must be class
                // TODO: check param is from sym
                if param.symbol.owner != sym then
                  report.error(i"the owner of $param must be $sym", tree.srcPos)
                if param.underlying.computeMutability != MutabilityQualifier.Readonly then
                  report.error(i"the mutability of $sym must be @Readonly", tree.srcPos)
              case _ =>
          case Apply(_, List(_: This)) =>
            // methods: if the result type refer this, then the owener must be class
            if !sym.owner.isClass then
              report.error(i"the owner of $sym must be class", tree.srcPos)
            if sym.findMutability != MutabilityQualifier.Readonly then
              report.error(i"the mutability of $sym must be @Readonly", tree.srcPos)
          case Apply(_, List(Select(id: Ident, _))) =>
            // check default value
            // difficult to do anything here, maybe add an attachment during desugar?
        MutabilityType(pt, MutabilityQualifier.Readonly)
      case pt => pt
    }

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Unit =
      if !tree.rhs.isEmpty then recheck(tree.rhs, insideViewOfPt(sym, sym.info, tree))

    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Unit =
      val rhsCtx = linkConstructorParams(sym).withOwner(sym)
      if !tree.rhs.isEmpty && !sym.isInlineMethod && !sym.isEffectivelyErased then
        inContext(rhsCtx) {
          val pt = insideViewOfPt(sym, recheck(tree.tpt), tree)
          recheck(tree.rhs, pt)
        }
