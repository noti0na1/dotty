package dotty.tools
package dotc
package mutability

import core._
import Phases.*, DenotTransformers.*, SymDenotations.*
import Contexts.*, Names.*, Flags.*, Symbols.*, Decorators.*
import Types.*, StdNames.*
import ast.tpd
import transform.Recheck.*

class MutabilitySetup(
  preRecheckPhase: DenotTransformer,
  thisPhase: DenotTransformer,
  recheckDef: (tpd.ValOrDefDef, Symbol) => Context ?=> Unit)
extends tpd.TreeTraverser:
  import tpd.*

  // Substitute parameter symbols in `from` to paramRefs in corresponding
  // method or poly types `to`. We use a single BiTypeMap to do everything.
  private class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
  extends DeepTypeMap:

    def apply(t: Type): Type = t match
      case t: NamedType =>
        val sym = t.symbol
        def outer(froms: List[List[Symbol]], tos: List[LambdaType]): Type =
          def inner(from: List[Symbol], to: List[ParamRef]): Type =
            if from.isEmpty then outer(froms.tail, tos.tail)
            else if sym eq from.head then to.head
            else inner(from.tail, to.tail)
          if tos.isEmpty then t
          else inner(froms.head, tos.head.paramRefs)
        outer(from, to)
      case _ =>
        mapOver(t)

    def inverse(t: Type): Type = t match
      case t: ParamRef =>
        def recur(from: List[LambdaType], to: List[List[Symbol]]): Type =
          if from.isEmpty then t
          else if t.binder eq from.head then to.head(t.paramNum).namedType
          else recur(from.tail, to.tail)
        recur(to, from)
      case _ =>
        mapOver(t)
  end SubstParams

  def traverse(tree: Tree)(using Context) =
    traverseChildren(tree)
    tree match
      case tree: ValOrDefDef =>
        val sym = tree.symbol
        val tpt = tree.tpt

        // replace an existing symbol info with inferred types
        def integrateRT(
            info: Type,                     // symbol info to replace
            psymss: List[List[Symbol]],     // the local (type and trem) parameter symbols corresponding to `info`
            prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
            prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
          ): Type =
          info match
            case mt: MethodOrPoly =>
              val psyms = psymss.head
              mt.companion(mt.paramNames)(
                mt1 =>
                  if !psyms.exists(_.isUpdatedAfter(preRecheckPhase)) && !mt.isParamDependent && prevLambdas.isEmpty then
                    mt.paramInfos
                  else
                    val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                    psyms.map(psym => subst(psym.info).asInstanceOf[mt.PInfo]),
                mt1 =>
                  integrateRT(mt.resType, psymss.tail, psyms :: prevPsymss, mt1 :: prevLambdas)
              )
            case info: ExprType =>
              info.derivedExprType(resType =
                integrateRT(info.resType, psymss, prevPsymss, prevLambdas))
            case _ =>
              val restp = tree.tpt.knownType
              if prevLambdas.isEmpty then restp
              else SubstParams(prevPsymss, prevLambdas)(restp)

        tpt.rememberType(ensureMutabilityType(tpt.tpe))

        if tree.tpt.hasRememberedType && !sym.isConstructor then
          val newInfo = integrateRT(sym.info, sym.paramSymss, Nil, Nil)
            // .showing(i"update info $sym: ${sym.info} --> $result", capt)
          if newInfo ne sym.info then
            val completer = new LazyType:
              def complete(denot: SymDenotation)(using Context) =
                denot.info = newInfo
                recheckDef(tree, sym)
            sym.updateInfoBetween(preRecheckPhase, thisPhase, completer)
      case _ =>

end MutabilitySetup
