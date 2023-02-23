package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Decorators.*, MutabilityOps.*

enum Mutability:
  case Readonly
  case Polyread // temporary solution for polyread
  case Refs(refs: Set[TypeRef])
  case Mutable

  def widen(refs: Set[TypeRef], isHigher: Boolean)(using Context): Mutability =
    refs.foldLeft(Mutable) { (mut, ref) =>
      mut.max(ref.info.computeMutability(isHigher))
    }

  def conforms(that: Mutability)(using Context): Boolean = that match
    case Readonly => true
    case Polyread if this == Polyread || this == Mutable => true
    case Mutable if this == Mutable => true
    case Refs(thatRefs) => this match
      case Mutable => true
      case Refs(thisRefs) =>
        val remainingThisRefs = thisRefs.filter { thisRef => !thatRefs.exists{ thatRef =>
          (thisRef.symbol ne NoSymbol)
          && (thisRef.symbol eq thatRef.symbol)
          // In theory, we should use `isPrefixSub` here,
          // but it's not exposed by `TypeComparer`.
          && (thisRef.prefix <:< thatRef.prefix)
        }}
        remainingThisRefs.isEmpty
        || widen(remainingThisRefs, isHigher = true).conforms(that)
        || Refs(remainingThisRefs).conforms(widen(thatRefs, isHigher = false))
      case _ =>
        this.conforms(widen(thatRefs, isHigher = false))
    case _ => this match
      case Refs(thisRefs) =>
        widen(thisRefs, isHigher = true).conforms(that)
      case _ => false

  def max(that: Mutability)(using Context): Mutability =
    that match
      case Readonly => return that
      case Refs(thatRefs) => this match
        case Readonly => return this
        case Refs(thisRefs) => return Refs(thisRefs ++ thatRefs)
        case _ =>
      case _ => this match
        case Readonly | Refs(_) => return this
        case _ =>
    if this.conforms(that) then that else this

