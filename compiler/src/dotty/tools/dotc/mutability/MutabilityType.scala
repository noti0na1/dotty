package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import MutabilityQualifier.*
import MutabilityOps.*


// abstract class MutabilityType

// enum ConcreteMutabilityType extends MutabilityType:
//   case Mutable(parent: Type) extends ConcreteMutabilityType
//   case Readonly(parent: Type) extends ConcreteMutabilityType

// case class PolyMutabilityType(parent: Type) extends MutabilityType

// case class RefMutabilityType(parent: Type, ref: Type) extends MutabilityType

object MutabilityType:
  def apply(parent: Type, mut: Mutability)(using Context): Type =
    (mut: @unchecked) match
      case mut: MutabilityQualifier => ConcreteMutabilityType(parent, mut)
      // TODO
      case mut: Set[SingletonType] => ConcreteMutabilityType(parent, Readonly)

end MutabilityType

object ConcreteMutabilityType:

  def apply(parent: Type, mut: MutabilityQualifier)(using Context): Type =
    val annot = mut match
      case Mutable => defn.MutableAnnot
      // case Polyread => defn.PolyreadAnnot
      case Readonly => defn.ReadonlyAnnot
    AnnotatedType(parent, Annotation(annot))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, MutabilityQualifier)] =
    tp.annot.getMutabilityQualifier.map((tp.parent, _))

end ConcreteMutabilityType

object PolyMutabilityType:

  def unapply(tp: AnnotatedType)(using Context): Option[Type] =
    if tp.annot.symbol == defn.PolyreadAnnot then Some(tp.parent)
    else None

end PolyMutabilityType

object RefMutabilityType:
  import ast.tpd.*

  // def apply(parent: Type, refs: Set[SingletonType])(using Context): Type =
  //   refs.foldLeft(parent) { (tp, ref) =>
  //     AnnotatedType(tp, ConcreteAnnotation(???))
  //   }

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, Type)] =
    if tp.annot.symbol == defn.RefMutAnnot then tp.annot.tree match
      case Apply(_, List(id: Ident)) =>
        Some((tp.parent, id.tpe))
      case Apply(_, List(ts: This)) =>
        Some((tp.parent, ts.tpe))
      case _ => None
    else None

end RefMutabilityType
