package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, MutabilityQualifier.*
import Ordering.Implicits._

object MutabilityOps:

  extension (annot: Annotation)
    def getMutabilityQualifier(using Context): Option[MutabilityQualifier] =
      val sym = annot.symbol
      if sym == defn.MutableAnnot then Some(Mutable)
      else if sym == defn.PolyreadAnnot then Some(Polyread)
      else if sym == defn.ReadonlyAnnot then Some(Readonly)
      else None

  extension (tp: Type)
    def computeMutability(using Context): MutabilityQualifier =
      def recur(tp: Type): MutabilityQualifier = tp.dealiasKeepMutabilityAnnots match
        // TODO: double check all types
        case MutabilityType(parent, mut) =>
          mut.max(recur(parent))
        case tp: AnnotatedType =>
          recur(tp.parent)
        case tp: AndOrType =>
          val tp1Mut = recur(tp.tp1)
          val tp2Mut = recur(tp.tp2)
          tp1Mut.max(tp2Mut)
        case tp: TypeRef =>
          recur(tp.info)
        case tp: TypeBounds =>
          // val lom = tp.lo.computeMutability
          // val him = tp.hi.computeMutability
          // if lom == him then lom
          // else Polyread
          tp.hi.computeMutability
        case tp: SingletonType =>
          recur(tp.underlying)
        case tp: ExprType =>
          recur(tp.resType)
        case tp: MatchType =>
          val tp1 = tp.reduced
          if tp1.exists then recur(tp1)
          else Mutable
        case _ => Mutable
      recur(tp)

    def stripMutability(using Context): Type = tp match
      case MutabilityType(parent, _) => parent.stripMutability
      case tp: AnnotatedType => tp.derivedAnnotatedType(tp.parent.stripMutability, tp.annot)
      case tp: AndOrType => tp.derivedAndOrType(tp.tp1.stripMutability, tp.tp2.stripMutability)
      case _ => tp

  extension (sym: Symbol)
    def findMutability(using Context): MutabilityQualifier =
      def recur(annots: List[Annotation]): MutabilityQualifier =
        annots match
          case Nil => Mutable
          case annot :: annots =>
            // TODO: multiple mutability annotations?
            annot.getMutabilityQualifier match
              case Some(mut) => mut
              case None => recur(annots)
      recur(sym.annotations)


