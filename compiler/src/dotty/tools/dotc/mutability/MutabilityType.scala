package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import MutabilityOps.*


// abstract class MutabilityType

// enum ConcreteMutabilityType extends MutabilityType:
//   case Mutable(parent: Type) extends ConcreteMutabilityType
//   case Readonly(parent: Type) extends ConcreteMutabilityType

// case class PolyMutabilityType(parent: Type) extends MutabilityType

// case class RefMutabilityType(parent: Type, ref: Type) extends MutabilityType

object MutabilityType:

  import ast.tpd.*

  def apply(tp: Type, mut: MutabilityAnnotation)(using Context): AnnotatedType =
    AnnotatedType(tp, mut)

  // do not go deeper and combine annotations
  def unapply(tp: Type)(using Context): Option[(Type, MutabilityAnnotation)] =
    tp match
      case AnnotatedType(parent, annot) =>
        annot.toMutabilityAnnotation match
          case Some(mut) => Some((parent, mut))
          case _ => None
      case _ => None

end MutabilityType
