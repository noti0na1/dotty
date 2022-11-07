package dotty.tools
package dotc
package mutability

import core.*
import ast.tpd
import Types.*, Symbols.*, Contexts.*, Annotations.*
import util.SimpleIdentitySet

object MutabilityOps:

  import tpd.*

  extension (m1: MutabilityAnnotation)

    def add(m2: MutabilityAnnotation) = m1 match
      case ReadonlyAnnotation => ReadonlyAnnotation
      case PolyreadAnnotation =>
        // TODO: error
        // ReadonlyAnnotation
        assert(false, "PolyreadAnnotation.add")
      case MutableAnnotation => m2
      case m1: ReferenceMutabilityAnnotation => m2 match
        case ReadonlyAnnotation => m2
        case PolyreadAnnotation =>
          // TODO: error
          // m2
          assert(false, ".add(PolyreadAnnotation)")
        case MutableAnnotation => m1
        case m2: ReferenceMutabilityAnnotation =>
          ReferenceMutabilityAnnotation(m1.refs ++ m2.refs)

    def conforms(m2: MutabilityAnnotation) = m1 match
      case ReadonlyAnnotation => m2 match
        case ReadonlyAnnotation => true
        case _ => false
      case PolyreadAnnotation =>
        // TODO: error
        // m2 match
        //   case ReadonlyAnnotation => true
        //   case _ => false
        assert(false, "PolyreadAnnotation.conforms")
      case MutableAnnotation => true
      case m1: ReferenceMutabilityAnnotation => m2 match
        case ReadonlyAnnotation => true
        case PolyreadAnnotation =>
          // TODO: error
          // true
          assert(false, ".conforms(PolyreadAnnotation)")
        case MutableAnnotation => false
        case m2: ReferenceMutabilityAnnotation => m1.refs.forall(m2.refs.contains)

  extension (tp: Type)

    def computeMutability(using Context): MutabilityAnnotation = tp.dealiasKeepMutabilityAnnots match
      // TODO: double check all types
      case MutabilityType(tp, mut) =>
        assert(mut != PolyreadAnnotation, "(T @polyread).computeMutability")
        tp.computeMutability.add(mut)
      case tp: AnnotatedType =>
        tp.parent.computeMutability
      case tp: AndOrType =>
        val tp1Mut = tp.tp1.computeMutability
        val tp2Mut = tp.tp2.computeMutability
        tp1Mut.add(tp2Mut)
      case tp: TypeRef =>
        tp.info.computeMutability
      case tp: TypeBounds =>
        tp.lo.computeMutability
      case tp: CaptureRef =>
        tp.underlying match
          case MutabilityType(tpParent, PolyreadAnnotation) =>
            tpParent.computeMutability.add(
              ReferenceMutabilityAnnotation(SimpleIdentitySet(tp)))
          case tp => tp.computeMutability
      case tp: SingletonType =>
        tp.underlying.computeMutability
      // case tp: ExprType =>
      //   recur(tp.resType)
      case tp: MatchType =>
        val tp1 = tp.reduced
        if tp1.exists then tp1.computeMutability
        else MutableAnnotation
      case _ => MutableAnnotation

    def substRef(ref: CaptureRef, mut: MutabilityAnnotation)(using Context): Type = tp.dealiasKeepMutabilityAnnots match
      case MutabilityType(tp, tpMut: ReferenceMutabilityAnnotation) =>
        val newTp = tp.substRef(ref, mut)
        if tpMut.refs.contains(ref) then
          val newRefs = tpMut.refs - ref
          if newRefs.isEmpty then
            MutabilityType(newTp, mut)
          else
            println(s"mut: $mut, newRefs: $newRefs")
            MutabilityType(newTp, mut.add(ReferenceMutabilityAnnotation(newRefs)))
        else
          MutabilityType(newTp, tpMut)
      case MutabilityType(tp, tpMut) =>
        MutabilityType(tp.substRef(ref, mut), tpMut)
      case tp: AnnotatedType =>
        tp.derivedAnnotatedType(tp.parent.substRef(ref, mut), tp.annot)
      case tp: AppliedType =>
        tp.derivedAppliedType(tp.tycon.substRef(ref, mut), tp.args.map(_.substRef(ref, mut)))
      case tp: LambdaType =>
        tp.derivedLambdaType(
          tp.paramNames,
          tp.paramInfos.map(_.substRef(ref, mut).asInstanceOf[tp.PInfo]),
          tp.resType.substRef(ref, mut))
      case tp: AndOrType =>
        tp.derivedAndOrType(tp.tp1.substRef(ref, mut), tp.tp2.substRef(ref, mut))
      // case tp: TypeRef =>
      //   tp.info.substRef(ref, mut)
      case tp: TypeBounds =>
        tp.derivedTypeBounds(tp.lo.substRef(ref, mut), tp.hi.substRef(ref, mut))
      case tp: SingletonType =>
        val tpw = tp.underlying
        val tp2 = tpw.substRef(ref, mut)
        if tpw eq tp2 then tp else
          AndType(tp, tp2)
      case tp: MatchType =>
        val tp1 = tp.reduced
        if tp1.exists then tp1.substRef(ref, mut) else tp
      case _ => tp

  extension (annot: Annotation)
    def toMutabilityAnnotation(using Context): Option[MutabilityAnnotation] =
      def getRefs(args: List[Tree], acc: SimpleIdentitySet[CaptureRef]) =
        args.foldLeft(acc) { case (acc, arg) =>
          arg.tpe match
            case ref: CaptureRef => acc + ref
            case _ =>
              // TODO: error, add checkWellFormed
              acc
        }
      annot match
        case mut: MutabilityAnnotation => Some(mut)
        case _ =>
          val sym = annot.symbol
          if sym == defn.MutableAnnot then Some(MutableAnnotation)
          else if sym == defn.PolyreadAnnot then Some(PolyreadAnnotation)
          else if sym == defn.ReadonlyAnnot then Some(ReadonlyAnnotation)
          else if sym == defn.RefMutAnnot then
            annot.tree match
              case Apply(_, List()) =>
                Some(MutableAnnotation)
              case Apply(_, List(Typed(SeqLiteral(args, _), _))) =>
                val refs = getRefs(args, SimpleIdentitySet.empty)
                if refs.isEmpty then None
                else Some(ReferenceMutabilityAnnotation(refs))
              case _ => None
          else None

  extension (sym: Symbol)
    def findMutability(using Context): MutabilityAnnotation =
      def recur(annots: List[Annotation]): MutabilityAnnotation =
        annots match
          case Nil => MutableAnnotation
          case annot :: annots =>
            // TODO: multiple mutability annotations?
            annot.toMutabilityAnnotation match
              case Some(mut) => mut
              case None => recur(annots)
      recur(sym.annotations)
