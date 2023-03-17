package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import MutabilityOps.*
import util.Spans.Span

object MutabilityType:

  def apply(parent: Type, mut: Type)(using Context): Type =
    if mut.isRef(defn.MutableClass) then parent
    else parent match
      case MutabilityType(parent1, mut1) =>
        if mut <:< mut1 then parent
        else apply(parent1, mut)
      case _ =>
        AnnotatedType(parent, MutabilityAnnotation(mut))

  // only find the mutability annotation on the top level
  def unapply(tp: AnnotatedType)(using Context): Option[(Type, Type)] = tp match
    case AnnotatedType(parent, MutabilityAnnotation(mut)) => Some((parent, mut))
    case _ => tp.annot.getMutability.map((tp.parent, _))

end MutabilityType
