package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import MutabilityQualifier.*
import MutabilityOps.*
import util.Spans.Span

object MutabilityType:

  def apply(parent: Type, mut: MutabilityQualifier)(using Context): Type =
    if mut == Mutable then parent
    else parent match
      case MutabilityType(parent1, mut1) =>
        if mut <= mut1 then parent
        else apply(parent1, mut)
      case _ =>
        val annot = mut match
          case Mutable => defn.MutableAnnot
          case Polyread => defn.PolyreadAnnot
          case Readonly => defn.ReadonlyAnnot
        AnnotatedType(parent, Annotation(annot, Span(0)))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, MutabilityQualifier)] =
    tp.annot.getMutabilityQualifier.map((tp.parent, _))

end MutabilityType
