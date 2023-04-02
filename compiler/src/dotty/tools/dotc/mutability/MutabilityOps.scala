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
        else if annot.symbol == defn.ReadonlyClass then Some(defn.ReadonlyType)
        else if annot.symbol == defn.PolyreadClass then Some(defn.PolyreadType)
        else if annot.symbol == defn.MutableClass then Some(defn.MutableType)
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
          if tp.symbol.isClass then
            if tp.symbol.isReadonlyClass then defn.ReadonlyType else defn.MutableType
          else recur(tp.info)
        case tp: TypeParamRef =>
          recur(TypeComparer.bounds(tp))
        case tp: MatchType =>
          val tp1 = tp.reduced
          if tp1.exists then recur(tp1)
          else recur(tp.bound)
        case tp: TypeBounds =>
          // In theory, we would expect the upper and lower bounds have the same mutability,
          // then we only need to compute mutability of one side.
          // However, if one side is left open (Any or Nothing), the other side may
          // have a different mutability.
          // For example, `T >: C @readonly`, in this case, the lower bound is readonly,
          // but the upper bound is mutable.
          // The best we can do is to handle the bound specifically when another side is `Nothing` or `Any`.
          if tp.hi.isRef(defn.AnyClass) then recur(tp.lo)
          else if tp.lo.isRef(defn.NothingClass) then recur(tp.hi)
          else recur(tp.hi)
        case tp: TypeProxy =>
          recur(tp.underlying)
        case tp: WildcardType =>
          recur(tp.optBounds)
        case _ =>
          // println(i"computeMutability unhandled: $tp")
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
      || sym == defn.Mirror_ProductClass
      // || sym == defn.Mirror_SumClass
      || sym == defn.EqualsClass
      || sym == defn.ProductClass
      || sym == defn.SerializableClass
      || sym == defn.ThrowableClass
      || sym == defn.ExceptionClass
      || sym == defn.RuntimeExceptionClass
      || sym == defn.ScalaStaticsModuleClass
      || sym == defn.ConversionClass
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


