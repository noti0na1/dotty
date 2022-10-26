package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, MutabilityQualifier.*
import Ordering.Implicits._

object MutabilityOps:

  type Mutability = MutabilityQualifier | Set[SingletonType]

  val defaultMutability: Mutability = Mutable

  extension (m1: Mutability)

    def add(m2: Mutability) = (m1: @unchecked) match
      case Readonly => Readonly
      case Mutable => m2
      case m1: Set[SingletonType] => (m2: @unchecked) match
        case Readonly => m2
        case Mutable => m1
        case m2: Set[SingletonType] => m1 ++ m2

    def conforms(m2: Mutability) = (m1: @unchecked) match
      case Readonly => (m2: @unchecked) match
        case Readonly => true
        case _ => false
      case Mutable => true
      case m1: Set[SingletonType] => (m2: @unchecked) match
        case Readonly => true
        case Mutable => false
        case m2: Set[SingletonType] => m1.forall(m2.contains)

  extension (tp: Type)
    def computeMutability(using Context): Mutability =
      tp.dealiasKeepMutabilityAnnots match
        // TODO: double check all types
        case ConcreteMutabilityType(parent, mut) =>
          parent.computeMutability.add(mut)
        case PolyMutabilityType(parent) =>
          MutabilityQualifier.Readonly
        case RefMutabilityType(parent, ref) =>
          parent.computeMutability.add(ref.computeMutability)
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
        case tp: SingletonType =>
          tp.underlying match
            case PolyMutabilityType(tpParent) =>
              tpParent.computeMutability.add(Set(tp))
            case tp => tp.computeMutability
        // case tp: ExprType =>
        //   recur(tp.resType)
        case tp: MatchType =>
          val tp1 = tp.reduced
          if tp1.exists then tp1.computeMutability
          else defaultMutability
        case _ => defaultMutability

  extension (annot: Annotation)
    def getMutabilityQualifier(using Context): Option[MutabilityQualifier] =
      val sym = annot.symbol
      if sym == defn.MutableAnnot then Some(Mutable)
      else if sym == defn.ReadonlyAnnot then Some(Readonly)
      else None

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

  // def readonlyNothingType(using Context): AnnotatedType =
  //   AnnotatedType(defn.NothingType, Annotation(defn.ReadonlyAnnot))

  // def polyreadNothingType(using Context): AnnotatedType =
  //   AnnotatedType(defn.NothingType, Annotation(defn.PolyreadAnnot))


