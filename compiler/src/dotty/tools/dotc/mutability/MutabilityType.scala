package dotty.tools
package dotc
package mutability

import core._
import Annotations._, Contexts._, Definitions._, Symbols._, Types._

object MutabilityType:

  def apply(parent: Type, qulifier: MutabilityQualifier)(using Context) =
    AnnotatedType(parent, MutabilityAnnotation(qulifier))

  def unapply(tp: Type)(using Context): Option[(Type, MutabilityQualifier)] = None
