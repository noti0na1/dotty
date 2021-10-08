package dotty.tools
package dotc
package mutability

import core._
import Contexts._, Definitions._
import Types._
import Symbols._
import Decorators._
import transform._
import MutabilityType.ensureMutabilityType

class CheckMutability extends Recheck:
  thisPhase =>

  import ast.tpd.*

  def phaseName: String = "CheckMutability"

  override def isEnabled(using Context) = true // TODO

  override def newRechecker()(using Context) = MutabilityChecker(ctx)

  class MutabilityChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

    // override def transformType(tp: Type, inferred: Boolean)(using Context): Type =
    //   // println(tp.show)
    //   tp

    override def recheckValDef(tree: ValDef, sym: Symbol)(using Context): Unit =
      sym.updateInfo(ensureMutabilityType(sym.info))
      super.recheckValDef(tree, sym)
    //   // println(i"recheckValDef: ${tree}")
    //   // if !tree.rhs.isEmpty then recheck(tree.rhs, ensureMutabilityType(sym.info))

    // override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Unit =
    //   // println(tree)
    //   // println(sym.info)
    //   tree.termParamss.foreach(_.foreach(recheck(_)))
    //   val tpt1 = TypeTree(ensureMutabilityType(tree.tpt.tpe))
    //   val tree1 = cpy.DefDef(tree)(tree.name, tree.paramss, tpt1, tree.rhs)
    //   super.recheckDefDef(tree1, sym)
    //   // val rhsCtx = linkConstructorParams(sym).withOwner(sym)
    //   // if !tree.rhs.isEmpty && !sym.isInlineMethod && !sym.isEffectivelyErased then
    //   //   inContext(rhsCtx) { recheck(tree.rhs, recheck(tree.tpt)) }