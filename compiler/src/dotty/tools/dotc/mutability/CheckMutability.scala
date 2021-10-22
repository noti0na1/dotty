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

class CheckMutability extends Recheck:
  thisPhase =>

  import ast.tpd.*

  def phaseName: String = "CheckMutability"

  override def isEnabled(using Context) = ctx.settings.Yiref.value

  override def newRechecker()(using Context) = MutabilityChecker(ctx)

  class MutabilityChecker(ictx: Context) extends Rechecker(ictx):
    import ast.tpd.*

    override def transformTypes(tree: Tree)(using Context) = myMutabilityTransformTypes.traverse(tree)

    object myMutabilityTransformTypes extends TreeTraverser:
      // Substitute parameter symbols in `from` to paramRefs in corresponding
      // method or poly types `to`. We use a single BiTypeMap to do everything.
      class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
      extends DeepTypeMap: //, BiTypeMap:

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
            // add mutability annotation to the type
            ensureMutabilityType(tpt.tpe).rememberFor(tpt)

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
                      if !psyms.exists(isUpdated) && !mt.isParamDependent && prevLambdas.isEmpty then
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
                  val restp = knownType(tree.tpt)
                  if prevLambdas.isEmpty then restp
                  else SubstParams(prevPsymss, prevLambdas)(restp)

            if tree.tpt.hasAttachment(RecheckedType) && !sym.isConstructor then
              val newInfo = integrateRT(sym.info, sym.paramSymss, Nil, Nil)
              // println(i"update info $sym: ${sym.info} --> $newInfo")
                // .showing(i"update info $sym: ${sym.info} --> $result", recheckr)
              if newInfo ne sym.info then
                val completer = new LazyType:
                  def complete(denot: SymDenotation)(using Context) =
                    denot.info = newInfo
                    recheckDef(tree, sym)
                sym.updateInfo(completer)
          // case tree: Bind =>
          //   println(tree)
          //   val sym = tree.symbol
          //   sym.updateInfo(ensureMutabilityType(sym.info))
          case _ =>
    end myMutabilityTransformTypes

    override def recheckSelect(tree: Select, pt: Type)(using Context): Type = tree match
      case Select(qual, name) =>
        // println(tree)
        var qualType = recheck(qual).widenIfUnstable

        if name.is(OuterSelectName) then return tree.tpe

        //val pre = ta.maybeSkolemizePrefix(qualType, name)
        var preQuli = qualType match
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
            if enclosingMethod != NoSymbol then preQuli = enclosingMethod.findMutability
          case _ =>

        val mbr = qualType.findMember(name, qualType,
            excluded = if tree.symbol.is(Private) then EmptyFlags else Private
          ).suchThat(tree.symbol ==)

        // check assign `x.a = ???`
        // the mutability of x must be Mutable
        if pt == AssignProto then
          // println(i"check assign $tree, $qualType with $preQuli")
          if preQuli > MutabilityQualifier.Mutable then
            report.error(i"trying to mutate a readonly field of $qual", tree.srcPos)

        // check method calls `x.f(...)`
        // the mutability of x must be less than or equal to the mutability of f
        val mbrSym = mbr.symbol
        if mbrSym.denot.isRealMethod && !mbrSym.denot.isStatic then
          val mbrQuli = mbrSym.findMutability
          if preQuli > mbrQuli then
          report.error(i"calling $mbrQuli $mbr on $preQuli $qual", tree.srcPos)

        val tp = constFold(tree, qualType.select(name, mbr))

        // when selecting field with polyread annotation
        if !mbrSym.is(Method) then
          tp match
            case MutabilityType(_, q) if q == MutabilityQualifier.Polyread =>
              MutabilityType(tp, preQuli)
            case _ => tp
        else tp
        //.showing(i"recheck select $qualType . $name : ${mbr.symbol.info} = $result")
