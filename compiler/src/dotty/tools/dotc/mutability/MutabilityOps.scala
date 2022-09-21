package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, MutabilityQualifier.*

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
    // Distribute Mutability annotations into And and Or types,
    // and combine multiple Mutability annotations into a single one.
    def normalizeMutabilityType(using Context): Type =
      def recur(t: Type, mut: MutabilityQualifier): Type =
        val tw = t.dealiasKeepMutabilityAnnots
        val tnorm = tw match
          case MutabilityType(tparent, mut2) =>
            recur(tparent, if mut < mut2 then mut2 else mut)
          case tw: AndOrType =>
            tw.derivedAndOrType(recur(tw.tp1, mut), recur(tw.tp2, mut))
          case tw: TypeBounds =>
            tw.derivedTypeBounds(recur(tw.lo, mut), recur(tw.hi, mut))
          case tw: TypeRef =>
            tw.info match {
              case TypeBounds(lo, _) =>
                if readonlyNothingType frozen_<:< lo then
                  MutabilityType(tw, Readonly)
                else if polyreadNothingType frozen_<:< lo then
                  MutabilityType(tw, Polyread)
                else
                  MutabilityType(tw, mut)
              case _ =>
                MutabilityType(tw, mut)
            }
          case tw =>
            MutabilityType(tw, mut)
        if tnorm eq tw then t else tnorm
      val r = recur(tp, Mutable)
      // println("normalizeMutabilityType " + tp.show + " -> " + r.show)
      r


