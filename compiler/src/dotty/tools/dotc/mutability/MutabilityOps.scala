package dotty.tools
package dotc
package mutability

import core._
import Annotations._, Contexts._, Definitions._, Symbols._, Types._
import MutabilityAnnotation._

extension (annot: Annotation)

  def isMutabilityAnnotation(using Context): Boolean =
    annot.isInstanceOf[MutabilityAnnotation] || isMutabilityAnnotationSymbol(annot.symbol)

extension (tp: Type)

  def hasMutabilityType(using Context): Boolean = tp match
    case tp: AnnotatedType =>
      if isMutabilityAnnotation(tp.annot) then true
      else tp.parent.hasMutabilityType
    case tp: TypeProxy =>
      tp.underlying.hasMutabilityType
    case tp: AndType =>
      tp.tp1.hasMutabilityType && tp.tp2.hasMutabilityType
    case tp: OrType =>
      tp.tp1.hasMutabilityType || tp.tp2.hasMutabilityType
    case _ => false

  def ensureMutabilityType(using Context): Type =
    if tp == NoType || tp.hasMutabilityType then tp
    else MutabilityType(tp, MutabilityQualifier.Mutable)

  def removeTopLeaveMutabilityType(using Context): Type = tp match
    case tp: AnnotatedType =>
      val parent = tp.parent.removeTopLeaveMutabilityType
      if isMutabilityAnnotation(tp.annot) then parent
      else tp.derivedAnnotatedType(parent, tp.annot)
    case tp: TypeProxy =>
      val tp0 = tp.underlying
      val tp1 = tp0.removeTopLeaveMutabilityType
      if tp0 eq tp1 then tp
      else tp1
    // case tp: AndType =>
    //   // TODO
    // case tp: OrType =>
    //   // TODO
    case _ => tp

  def setMutability(q: MutabilityQualifier)(using Context): Type =
    if tp == NoType then tp
    else MutabilityType(tp.removeTopLeaveMutabilityType, q)

extension (sym: Symbol)

  def findMutability(using Context): MutabilityQualifier =
    sym.annotations.map(_.symbol)
      .find(isMutabilityAnnotationSymbol)
      .map(mutabilitySymbolToQualifier)
      .getOrElse(MutabilityQualifier.Mutable)