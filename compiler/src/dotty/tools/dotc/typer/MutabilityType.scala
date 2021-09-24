package dotty.tools.dotc
package typer

import core._
import Annotations._, Contexts._, Definitions._, Symbols._, Types._

object MutabilityType:
  type Degree = Int

  val ReadonlyDegree: Degree = 3

  val PolyreadDegree: Degree = 2

  val MutableDegree: Degree = 1

  def annotToMutability(annot: Annotation)(using Context): Option[Degree] =
    val sym = annot.symbol
    if sym.derivesFrom(defn.ReadonlyAnnot) then Some(ReadonlyDegree)
    else if sym.derivesFrom(defn.MutableAnnot) then Some(MutableDegree)
    else None

  def apply(tp: Type, d: Degree)(using Context) =
    AnnotatedType(tp, ???)

  def unapply(tp: Type)(using Context): Option[(Type, Degree)] = tp.dealias.widen match
    case AnnotatedType(tp, annot) => annotToMutability(annot).map((tp, _))
    case _ => None
