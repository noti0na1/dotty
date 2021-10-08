package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import ast.Trees.*
import ast.{tpd, untpd}
import MutabilityQualifier.*

case class MutabilityAnnotation(qualifier: MutabilityQualifier) extends Annotation:

  import tpd.*

  override def symbol(using Context) = qualifier match
    case Mutable => defn.MutableAnnot
    case Polyread => defn.PolyreadAnnot
    case Readonly => defn.ReadonlyAnnot

  override def tree(using Context) = New(symbol.typeRef, Nil)

  override def derivedAnnotation(tree: Tree)(using Context): Annotation =
    unsupported("derivedAnnotation(Tree)")

  def derivedAnnotation(qualifier: MutabilityQualifier)(using Context): Annotation =
    if this.qualifier eq qualifier then this
    else MutabilityAnnotation(qualifier)

  override def sameAnnotation(that: Annotation)(using Context): Boolean = that match
    case MutabilityAnnotation(qualifier2) => qualifier == qualifier2
    case _ => false

  override def hash: Int = qualifier.d

  override def eql(that: Annotation) = that match
    case that: MutabilityAnnotation => this.qualifier eq that.qualifier
    case _ => false

end MutabilityAnnotation

object MutabilityAnnotation:
  def isMutabilityAnnotationSymbol(sym: Symbol)(using Context): Boolean =
    sym == defn.MutableAnnot
    || sym == defn.PolyreadAnnot
    || sym == defn.ReadonlyAnnot

  def mutabilitySymbolToQualifier(sym: Symbol)(using Context): MutabilityQualifier =
    if sym == defn.MutableAnnot then Mutable
    else if sym == defn.PolyreadAnnot then Polyread
    else if sym == defn.ReadonlyAnnot then Readonly
    else throw Exception("wrong symbol")
