package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Decorators.*, MutabilityOps.*
import printing.{Showable, Printer}
import printing.Texts.*

enum Mutability extends Showable:
  case Readonly
  case Polyread // temporary solution for polyread
  case Refs(refs: Set[Type])
  case Mutable

  def widen(tps: Set[Type], isHigher: Boolean)(using Context): Mutability =
    tps.foldLeft(Mutable) { (mut, tp) =>
      (tp match
        case ref: NamedType => ref.info match
          case TypeBounds(lo, hi) =>
            if isHigher then hi else lo
          case info => info
        case _ => tp).computeMutability.max(mut)
    }

  def conforms(that: Mutability)(using Context): Boolean = that match
    case Readonly => true
    case Polyread if this == Polyread || this == Mutable => true
    // If we are sure the refs only contain type references
    // which have different mutabilities on lower and higher bounds,
    // we can optimize this case, because !Refs(_).conforms(Mutable)
    case Mutable if this == Mutable => true
    case Refs(thatRefs) => this match
      case Mutable => true
      case Refs(thisRefs) =>
        val remainingThisRefs = thisRefs.filter { thisRef => !thatRefs.exists { thatRef =>
          (thisRef != NoType) && (thisRef eq thatRef)
          || { (thisRef, thatRef) match
            case (thisRef: NamedType, thatRef: NamedType) =>
              (thisRef.symbol ne NoSymbol)
              && (thisRef.symbol eq thatRef.symbol)
              // In theory, we should use `isPrefixSub` here,
              // but it's not exposed by `TypeComparer`.
              && (thisRef.prefix <:< thatRef.prefix)
            case _ => false
          }
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

  override def toText(printer: Printer): Text = this match
    case Readonly => Str("readonly")
    case Polyread => Str("polyread")
    case Refs(refs) =>
      Str("refs{") ~ Text(refs.toList.map(printer.toText), ", ") ~ Str("}")
    case Mutable => Str("mutable")
