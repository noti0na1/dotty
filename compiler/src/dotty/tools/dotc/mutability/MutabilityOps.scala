package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, MutabilityQualifier.*
import Ordering.Implicits._

object MutabilityOps:

  def readonlyNothingType(using Context): AnnotatedType =
    AnnotatedType(defn.NothingType, Annotation(defn.ReadonlyAnnot))

  def polyreadNothingType(using Context): AnnotatedType =
    AnnotatedType(defn.NothingType, Annotation(defn.PolyreadAnnot))

  extension (annot: Annotation)
    def toMutabilityQualifier(using Context): Option[MutabilityQualifier] =
      val sym = annot.symbol
      if sym == defn.MutableAnnot then Some(Mutable)
      else if sym == defn.PolyreadAnnot then Some(Polyread)
      else if sym == defn.ReadonlyAnnot then Some(Readonly)
      else None

  extension (tp: Type)
    def computeMutability(using Context): MutabilityQualifier =
      def recur(tp: Type): MutabilityQualifier = tp.dealiasKeepMutabilityAnnots match
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
          recur(tp.hi)
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


