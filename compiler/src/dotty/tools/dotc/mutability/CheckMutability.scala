package dotty.tools
package dotc
package mutability

import core._
import Contexts._, Definitions._
import Types._
import Symbols._
import Decorators._
import SymDenotations._
import Flags._
import NameKinds._
import typer.ProtoTypes._
import transform._
import Recheck._
import annotation.tailrec
import typer.RefChecks

object CheckMutability

class CheckMutability extends Recheck:
  thisPhase =>

  import ast.tpd.*

  def phaseName: String = "CheckMutability"

  override def isEnabled(using Context) = ctx.settings.Yiref.value

  override def newRechecker()(using Context) = MutabilityChecker(ctx)

  override def run(using Context): Unit =
    // checkOverrides.traverse(ctx.compilationUnit.tpdTree)
    super.run

  def checkOverrides = new TreeTraverser:
    def traverse(t: Tree)(using Context) =
      t match
        case t: Template =>
          RefChecks.checkAllOverrides(ctx.owner.asClass)
        case _ =>
      traverseChildren(t)

  class MutabilityChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

    override def recheckSelect(tree: Select, pt: Type)(using Context): Type =
      val qual = tree.qualifier
      val name = tree.name
      var qualType = recheck(qual).widenIfUnstable

      if name.is(OuterSelectName) then return tree.tpe

      var quilm = qualType match
        case MutabilityType(qt, q) => qualType = qt; q
        case _ => MutabilityQualifier.Mutable

      // if x is `C.this`, get mutability from enclosing method in C
      qual match
        case qual: This =>
          val cs = qual.symbol

          @tailrec def searchEnclosing(s: Symbol): Symbol =
            if s == NoSymbol || s.isRealMethod && s.owner == cs then s
            else if !s.is(Method) && s.owner == cs then cs.primaryConstructor // TODO
            else if s.exists then searchEnclosing(s.owner)
            else NoSymbol

          val enclosingMethod = searchEnclosing(ctx.owner)
          // println(cs)
          // println(enclosingMethod)
          if enclosingMethod != NoSymbol then
            quilm = enclosingMethod.findMutability
        case _ =>

      val mbr = qualType.findMember(name, qualType,
          excluded = if tree.symbol.is(Private) then EmptyFlags else Private
        ).suchThat(tree.symbol == _)

      // check assign `x.a = ???`
      // the mutability of x must be Mutable
      if pt == AssignProto then
        // println(i"check assign $tree, $qualType with $preQuli")
        if quilm > MutabilityQualifier.Mutable then
          report.error(i"trying to mutate a readonly field of $qual", tree.srcPos)

      // check method calls `x.f(...)`
      // the mutability of x must be less than or equal to the mutability of f
      val mbrSym = mbr.symbol
      if mbrSym.isRealMethod && !mbrSym.isStatic then
        val mbrm = mbrSym.findMutability
        if quilm > mbrm then
          report.error(i"calling $mbrm $mbr on $quilm $qual", tree.srcPos)

      val tp = constFold(tree, qualType.select(name, mbr))

      // when selecting field with polyread annotation
      if !mbrSym.is(Method) then
        tp match
          case MutabilityType(_, q) if q == MutabilityQualifier.Polyread =>
            MutabilityType(tp, quilm)
          case _ => tp
      else tp // TODO
      //.showing(i"recheck select $qualType . $name : ${mbr.symbol.info} = $result")

    // override def recheckApply(tree: Apply, pt: Type)(using Context): Type =
    //   ???

    override def checkUnit(unit: CompilationUnit)(using Context): Unit =
      MutabilitySetup(preRecheckPhase, thisPhase, recheckDef)
        .traverse(ctx.compilationUnit.tpdTree)
      super.checkUnit(unit)

  end MutabilityChecker

end CheckMutability
