package dotty.tools
package dotc
package mutability

import core._
import Annotations._, Contexts._, Definitions._, Symbols._, Types._
import MutabilityAnnotation._

object MutabilityType:

  def apply(parent: Type, qulifier: MutabilityQualifier)(using Context) =
    AnnotatedType(parent, MutabilityAnnotation(qulifier))

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, MutabilityQualifier)] =
    if ctx.phase == Phases.checkMutabilityPhase && isMutabilityAnnotationSymbol(tp.annot.symbol) then
      tp.annot match
        case ann: MutabilityAnnotation => Some((tp.parent, ann.qualifier))
        case ann => Some((tp.parent, mutabilitySymbolToQualifier(tp.annot.symbol)))
    else None

  def ensureMutabilityType(tp: Type)(using Context): AnnotatedType = tp match
    case tp: AnnotatedType if isMutabilityAnnotationSymbol(tp.annot.symbol) => tp
    case _ => apply(tp, MutabilityQualifier.Mutable)
