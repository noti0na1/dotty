package dotty.tools
package dotc
package mutability

import core._
import Annotations._, Contexts._, Definitions._, Symbols._, Types._
import MutabilityAnnotation._

extension (tp: Type)

  def hasMutabilityType(using Context): Boolean = tp match
    case tp: AnnotatedType =>
      if isMutabilityAnnotationSymbol(tp.annot.symbol) then true
      else tp.parent.hasMutabilityType
    case tp: TypeProxy =>
      tp.underlying.hasMutabilityType
    case tp: AndOrType =>
      tp.tp1.hasMutabilityType && tp.tp2.hasMutabilityType
    case _ => false

  def ensureMutabilityType(using Context): Type =
    if tp == NoType || tp.hasMutabilityType then tp
    else MutabilityType(tp, MutabilityQualifier.Mutable)
