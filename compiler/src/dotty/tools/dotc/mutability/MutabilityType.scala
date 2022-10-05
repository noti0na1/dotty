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
      // case Polyread => defn.PolyreadAnnot
      case Readonly => defn.ReadonlyAnnot
    AnnotatedType(parent, Annotation(annot))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, MutabilityQualifier)] =
    tp.annot.getMutabilityQualifier.map((tp.parent, _))

end MutabilityType
