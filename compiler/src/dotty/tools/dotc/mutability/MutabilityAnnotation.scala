package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import ast.Trees.*
import ast.{tpd, untpd}
import Decorators.*
import config.Printers.capt
import printing.Printer
import printing.Texts.*
import Mutability.*

/** An annotation representing a mutability
 *  It simulates a normal mutability annotation except that it is more efficient.
 *  These annotations are created during mutability checking. Before that
 *  there are only regular mutability annotations.
 */
case class MutabilityAnnotation(mut: Mutability) extends Annotation:
  import tpd.*

  override def tree(using Context) =
    New(symbol.typeRef, Nil)

  override def symbol(using Context) = mut match
    case Mutable => defn.MutableAnnot
    case Polyread => defn.PolyreadAnnot
    // TODO: add other annotation for Refs? like @specialized?
    case _: Refs => defn.PolyreadAnnot
    case Readonly => defn.ReadonlyAnnot

  override def derivedAnnotation(tree: Tree)(using Context): Annotation = this

  def derivedAnnotation(mut: Mutability)(using Context): Annotation =
    if this.mut == mut then this
    else MutabilityAnnotation(mut)

  override def sameAnnotation(that: Annotation)(using Context): Boolean = that match
    case MutabilityAnnotation(mut) => this.mut == mut
    case _ => false

  // override def mapWith(tm: TypeMap)(using Context) = mut match
  //   case Refs(refs) =>
  //     // TODO
  //     val elems = refs.toList
  //     val elems1 = elems.mapConserve(tm)
  //     if elems1 eq elems then this
  //     else if elems1.forall(_.isInstanceOf[CaptureRef])
  //     then derivedAnnotation(Refs(elems1.toSet))
  //     else EmptyAnnotation
  //   case _ => this

  override def toText(printer: Printer): Text = Str("@") ~ mut.toText(printer)

  override def eql(that: Annotation) = that match
    case that: MutabilityAnnotation => this.mut == that.mut
    case _ => false

end MutabilityAnnotation
