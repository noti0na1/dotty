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
    // checkOverrides.traverse(ctx.compilationUnit.tpdTree)
    super.run

  // def checkOverrides = new TreeTraverser:
  //   def traverse(t: Tree)(using Context) =
  //     t match
  //       case t: Template => checkAllOverrides(ctx.owner.asClass)
  //       case _ =>
  //     traverseChildren(t)

  class MutabilityChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

    override def recheckSelection(tree: Select, qualType: Type, name: Name, pt: Type)(using Context) = {
      val selType = super.recheckSelection(tree, qualType, name, pt)
      val qual = tree.qualifier
      var qualMut = qualType.computeMutability()

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

      // check assign `x.a = ???`
      // the mutability of x must be Mutable
      if pt == AssignProto then
        // println(i"check assign $tree, $qualType with $preQuli")
        if qualMut > MutabilityQualifier.Mutable then
          report.error(i"trying to mutate a readonly field of $qual", tree.srcPos)

      // check method calls `x.f(...)`
      // the mutability of x must be less than or equal to the mutability of f
      val mbr = qualType.findMember(name, qualType).suchThat(tree.symbol == _)
      val mbrSym = mbr.symbol
      if mbrSym.isRealMethod && !mbrSym.isStatic then
        val mbrMut = mbrSym.findMutability
        if qualMut > mbrMut then
          report.error(i"calling $mbrMut $mbr on $qualMut $qual", tree.srcPos)

      if !mbrSym.is(Method)
        && selType.computeMutability(MutabilityQualifier.Polyread) == MutabilityQualifier.Polyread then
          MutabilityType(selType, qualMut)
      else selType
    }

