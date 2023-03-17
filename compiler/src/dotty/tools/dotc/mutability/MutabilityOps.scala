package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Decorators.*
import Ordering.Implicits.*
import ast.Trees.*
import ast.{tpd, untpd}

object MutabilityOps:

  import tpd.*

  def isCheckingMutability(using Context): Boolean =
    ctx.settings.Ymut.value && ctx.phase == Phases.checkMutabilityPhase

  extension (annot: Annotation)
    def getMutability(using Context): Option[Type] = annot match
      case MutabilityAnnotation(mut) => Some(mut)
      case _ =>
        if annot.symbol == defn.MutAnnot then
          annot.tree match
            case Apply(TypeApply(_, List(tptree: TypeTree)), _) =>
              Some(tptree.tpe)
            case _ => None
        else None

  extension (tp: Type)

    def union(that: Type)(using Context): Type =
      if tp.isRef(defn.MutableClass) then that
      else if tp.isRef(defn.ReadonlyClass) then tp
      else if that.isRef(defn.MutableClass) then tp
      else if that.isRef(defn.ReadonlyClass) then that
      else OrType(tp, that, soft = false)

     /** @pre `tp` is a CapturingType */
    def derivedMutabilityType(parent: Type, mut: Type)(using Context): Type = tp match
      case tp @ MutabilityType(p, m) =>
        if (parent eq p) && (mut eq m) then tp
        else MutabilityType(parent, mut)

    def computeMutability(using Context): Type =
      def recur(tp: Type): Type = tp.dealiasKeepAnnots match
        case MutabilityType(parent, mut) =>
          recur(parent).union(mut)
        case tp: AnnotatedType =>
          // TODO: consider repeated params as readonly?
          // if tp.annot.symbol == defn.RepeatedAnnot then Readonly
          // else recur(tp.parent)
          recur(tp.parent)
        case tp: AndType =>
          val tp1Mut = recur(tp.tp1)
          val tp2Mut = recur(tp.tp2)
          AndType(tp1Mut, tp2Mut)
        case tp: OrType =>
          val tp1Mut = recur(tp.tp1)
          val tp2Mut = recur(tp.tp2)
          tp1Mut.union(tp2Mut)
        case tp: NamedType =>
          tp.info match
            case TypeBounds(lo, hi) =>
              val loMut = recur(lo)
              val hiMut = recur(hi)
              if loMut =:= hiMut then loMut
              else
                report.error(i"$tp has different mutability bounds $loMut and $hiMut")
                NoType
            case info => recur(info)
        case tp: TypeParamRef =>
          TypeComparer.bounds(tp) match
            case TypeBounds(lo, hi) =>
              val loMut = recur(lo)
              val hiMut = recur(hi)
              if loMut =:= hiMut then loMut
              else
                report.error(i"$tp has different mutability bounds $loMut and $hiMut")
                NoType
        case tp: SingletonType =>
          recur(tp.underlying)
        case tp: ExprType =>
          recur(tp.resType)
        case tp: MatchType =>
          val tp1 = tp.reduced
          if tp1.exists then recur(tp1)
          else defn.MutableType
        case tp: ClassInfo =>
          // println(i"computeMutability: $tp, ${tp.classSymbol.isReadonlyClass}")
          if tp.classSymbol.isReadonlyClass then defn.ReadonlyType else defn.MutableType
        case _ =>
          defn.MutableType
      recur(tp)

  extension (sym: Symbol)

    def findMutability(using Context): Type =
      def recur(annots: List[Annotation]): Type =
        annots match
          case Nil => defn.MutableType
          case annot :: annots =>
            // TODO: multiple mutability annotations?
            annot.getMutability match
              case Some(mut) => mut
              case None => recur(annots)
      if !sym.isClass && sym.owner.isReadonlyClass
        || defn.pureMethods.contains(sym)
      then defn.ReadonlyType
      else
        val r = recur(sym.annotations)
        // println(s"findMutability: $sym => $r")
        r

    def isReadonlyClass(using Context): Boolean =
      sym.isValueClass
      || sym == defn.CharSequenceClass
      || sym == defn.StringClass
      || sym == defn.Mirror_SingletonClass
      || sym == defn.EqualsClass
      || sym == defn.ProductClass
      || sym == defn.SerializableClass
      || sym == defn.ThrowableClass
      || sym == defn.ExceptionClass
      || sym == defn.RuntimeExceptionClass
      || sym == defn.ScalaStaticsModuleClass
      || sym == defn.ConversionClass
      || sym == defn.Mirror_SingletonClass
      || defn.isFunctionSymbol(sym)
      || sym.isClass && (sym.findMutability.isRef(defn.ReadonlyClass))

    def relaxApplyCheck(using Context): Boolean =
      val owner = sym.owner
      sym.is(Flags.Synthetic)
      || defn.pureMethods.contains(sym)
      || owner == defn.ScalaStaticsModuleClass
      || owner == defn.OptionClass
      || sym == defn.Any_asInstanceOf
      || sym == defn.Any_typeCast
      || sym == defn.Array_apply
      || sym == defn.Array_length
      // uncommant the following line if needed
      || owner == defn.IterableOpsClass
      || owner == defn.SeqOpsClass
      || owner == defn.IntegralProxyClass


