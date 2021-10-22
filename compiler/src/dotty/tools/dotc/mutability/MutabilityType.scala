package dotty.tools
package dotc
package mutability

import core._
import Annotations._, Contexts._, Definitions._, Symbols._, Types._
import MutabilityAnnotation._

object MutabilityType:

  def apply(parent: Type, qulifier: MutabilityQualifier)(using Context) =
    AnnotatedType(parent, MutabilityAnnotation(qulifier))

  def unapply(tp: Type)(using Context): Option[(Type, MutabilityQualifier)] =
    if ctx.phase != Phases.checkMutabilityPhase then None
    else tp match
      case tp: AnnotatedType if isMutabilityAnnotation(tp.annot) =>
        val tp1 = tp.parent.removeTopLeaveMutabilityType
        tp.annot match
          case ann: MutabilityAnnotation => Some((tp1, ann.qualifier))
          case ann => Some((tp1, mutabilitySymbolToQualifier(tp.annot.symbol)))
      case tp: AnnotatedType =>
        unapply(tp.parent).map { (tp1, q) =>
          (tp.derivedAnnotatedType(tp1, tp.annot), q)
        }
      case tp: TypeProxy =>
        unapply(tp.underlying)
      case _ => None
