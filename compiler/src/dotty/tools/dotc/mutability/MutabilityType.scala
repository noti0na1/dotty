package dotty.tools
package dotc
package mutability

import core._
import Annotations._, Contexts._, Definitions._, Symbols._, Types._
import MutabilityAnnotation._

object MutabilityType:

  def apply(parent: Type, qulifier: MutabilityQualifier)(using Context) =
    AnnotatedType(parent, MutabilityAnnotation(qulifier))

  // def unapply(tp: AnnotatedType)(using Context): Option[(Type, MutabilityQualifier)] =
  //   if ctx.phase == Phases.checkMutabilityPhase && isMutabilityAnnotationSymbol(tp.annot.symbol) then
  //     val parent = tp.parent.removeTopLeaveMutabilityType
  //     tp.annot match
  //       case ann: MutabilityAnnotation => Some((parent, ann.qualifier))
  //       case ann => Some((parent, mutabilitySymbolToQualifier(tp.annot.symbol)))
  //   else None

  def unapply(tp: Type)(using Context): Option[(Type, MutabilityQualifier)] =
    if ctx.phase != Phases.checkMutabilityPhase then return None
    tp match
      case AnnotatedType(tp1, annot) if isMutabilityAnnotation(annot) =>
        val tp2 = tp1.removeTopLeaveMutabilityType
        annot match
          case ann: MutabilityAnnotation => Some((tp2, ann.qualifier))
          case ann => Some((tp2, mutabilitySymbolToQualifier(annot.symbol)))
      case tp @ AnnotatedType(parent, annot) =>
        unapply(parent).map { (tp1, q) =>
          (tp.derivedAnnotatedType(tp1, annot), q)
        }
      case tp: TypeProxy =>
        unapply(tp.underlying)
      case _ => None
