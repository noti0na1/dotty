package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, StdNames.*
import ast.Trees.*
import ast.{tpd, untpd}
import Decorators.*
import config.Printers.*
import printing.Printer
import printing.Texts.*
import MutabilityOps.*

/** An annotation representing a mutability
 *  It simulates a normal mutability annotation except that it is more efficient.
 *  These annotations are created during mutability checking. Before that
 *  there are only regular mutability annotations.
 */
case class MutabilityAnnotation(mut: Type) extends Annotation:
  import tpd.*

  override def tree(using Context) = New(ref(defn.MutAnnot))
      // TODO: why this doesn't work?
    //   case Refs(refs) =>
    //     val arg = defn.tupleType(refs.toList)
    //     val argTree = refs.toList.map {
    //       case cr: TermRef => ref(cr)
    //       case cr: TermParamRef => untpd.Ident(cr.paramName).withType(cr)
    //       case cr: ThisType => This(cr.cls)
    //       case tp => TypeTree(tp)
    //     }
    //     val constructor = New(AppliedTypeTree(ref(defn.RefmutAnnot), List(tupleTypeTree(argTree)))).select(nme.CONSTRUCTOR)
    //     TypeApply(constructor, List(TypeTree(arg)))

  override def symbol(using Context) = defn.MutAnnot

  override def derivedAnnotation(tree: Tree)(using Context): Annotation = this

  def derivedAnnotation(mut: Type)(using Context): Annotation =
    if this.mut eq mut then this
    else MutabilityAnnotation(mut)

  override def sameAnnotation(that: Annotation)(using Context): Boolean = that match
    case MutabilityAnnotation(mut) => this.mut == mut
    case _ => false

  override def mapWith(tm: TypeMap)(using Context) =
    derivedAnnotation(tm(mut))

  override def toText(printer: Printer): Text = Str("@mut[") ~ mut.toText(printer) ~ Str("]")

  override def eql(that: Annotation) = that match
    case that: MutabilityAnnotation => this.mut eq that.mut
    case _ => false

end MutabilityAnnotation
