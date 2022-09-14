package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import MutabilityQualifier.*
import MutabilityOps.*

object MutabilityType:

  def apply(parent: Type, mut: MutabilityQualifier)(using Context): Type =
    val annot = mut match
      case Mutable => defn.MutableAnnot
      case Polyread => defn.PolyreadAnnot
      case Readonly => defn.ReadonlyAnnot
    AnnotatedType(parent, Annotation(annot))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, MutabilityQualifier)] =
    // if ctx.phase == Phases.checkMutabilityPhase then None else
    val r = tp.annot.toMutabilityQualifier.map((tp.parent, _))
    // println(s"unapply MutabilityType $tp -> $r")
    // throw new Exception("unapply MutabilityType")
    r

end MutabilityType
