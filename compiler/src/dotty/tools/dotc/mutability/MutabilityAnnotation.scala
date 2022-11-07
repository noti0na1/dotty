package dotty.tools
package dotc
package mutability

import core.*
import ast.{tpd, untpd}
import tpd.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import util.SimpleIdentitySet
import Decorators.*
import printing.{Showable, Printer}
import printing.Texts.*

abstract class MutabilityAnnotation extends Annotation:

  def isConcrete: Boolean

  override def derivedAnnotation(tree: Tree)(using Context): Annotation =
    unsupported(i"MutabilityAnnotation.derivedAnnotation(Tree), $tree")

end MutabilityAnnotation

abstract class ConcreteMutabilityAnnotation extends MutabilityAnnotation:

  def isConcrete: Boolean = true

  override def tree(using Context) =
    New(symbol.typeRef, Nil)

end ConcreteMutabilityAnnotation

object MutableAnnotation extends ConcreteMutabilityAnnotation:

  override def symbol(using Context) = defn.MutableAnnot

  override def toText(printer: Printer): Text = "@mutable"

object PolyreadAnnotation extends ConcreteMutabilityAnnotation:

  override def symbol(using Context) = defn.PolyreadAnnot

  override def toText(printer: Printer): Text = "@polyread"

object ReadonlyAnnotation extends ConcreteMutabilityAnnotation:

  override def symbol(using Context) = defn.ReadonlyAnnot

  override def toText(printer: Printer): Text = "@readonly"


case class ReferenceMutabilityAnnotation(refs: SimpleIdentitySet[CaptureRef]) extends MutabilityAnnotation:

  def isConcrete: Boolean = false

  override def tree(using Context) =
    val elems = refs.toList.map {
      case cr: TermRef => ref(cr)
      case cr: TermParamRef => untpd.Ident(cr.paramName).withType(cr)
      case cr: ThisType => This(cr.cls)
    }
    val arg = repeated(elems, TypeTree(defn.AnyType))
    New(symbol.typeRef, arg :: Nil)

  override def symbol(using Context) = defn.RefMutAnnot

  override def toText(printer: Printer): Text =
    Str("@refmut(") ~ Text(refs.toList.map(printer.toTextCaptureRef), ", ") ~ Str(")")

end ReferenceMutabilityAnnotation
