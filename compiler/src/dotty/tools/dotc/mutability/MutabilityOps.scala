package dotty.tools
package dotc
package mutability

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Mutability.*, Decorators.*
import Ordering.Implicits.*
import ast.Trees.*
import ast.{tpd, untpd}

object MutabilityOps:

  import tpd.*

  extension (annot: Annotation)
    def getMutability(using Context): Option[Mutability] = annot match
      case MutabilityAnnotation(mut) => Some(mut)
      case _ =>
        val sym = annot.symbol
        if sym == defn.MutableAnnot then Some(Mutable)
        else if sym == defn.PolyreadAnnot then Some(Polyread)
        else if sym == defn.ReadonlyAnnot then Some(Readonly)
        else if sym == defn.RefmutAnnot then
          annot.tree match
            case Apply(TypeApply(_, List(ttree: TypeTree)), _) =>
              defn.tupleTypes(ttree.tpe) match
                case Some(tpes) =>
                  // return Some(Refs(tpes.toSet))
                  // println(i"RefmutAnnot: ${tpes} => ${tpes.toRefs}")
                  return Some(tpes.toRefs)
                case _ =>
            case _ =>
          Some(Mutable)
        else None

  extension (tp: Type)

    def computeMutability(using Context): Mutability =
      def recur(tp: Type): Mutability = tp.dealiasKeepAnnots match
        case MutabilityType(parent, mut) =>
          mut.max(recur(parent))
        case tp: AnnotatedType =>
          // TODO: consider repeated params as readonly
          // if tp.annot.symbol == defn.RepeatedAnnot then Readonly
          // else recur(tp.parent)
          recur(tp.parent)
        case tp: AndOrType =>
          val tp1Mut = recur(tp.tp1)
          val tp2Mut = recur(tp.tp2)
          tp1Mut.max(tp2Mut)
        case tp: NamedType =>
          tp.info match
            case TypeBounds(lo, hi) =>
              val loMut = recur(lo)
              val hiMut = recur(hi)
              if loMut == hiMut then loMut else Refs(Set(tp))
            case info => recur(info)
        case tp: SingletonType =>
          recur(tp.underlying)
        case tp: ExprType =>
          recur(tp.resType)
        case tp: MatchType =>
          val tp1 = tp.reduced
          if tp1.exists then recur(tp1)
          else Mutable
        case tp: ClassInfo =>
          if tp.classSymbol.isReadonlyClass then Readonly else Mutable
        case _ =>
          Mutable
      recur(tp)

    def stripMutability(using Context): Type = tp match
      case MutabilityType(parent, _) => parent.stripMutability
      case tp: AnnotatedType => tp.derivedAnnotatedType(tp.parent.stripMutability, tp.annot)
      case tp: AndOrType => tp.derivedAndOrType(tp.tp1.stripMutability, tp.tp2.stripMutability)
      case _ => tp

  extension (tps: Seq[Type])

    def toRefs(using Context): Mutability =
      tps.foldLeft(Mutable) {
        case (mut, tp) => mut.max(tp.computeMutability)
      }

  extension (sym: Symbol)

    def findMutability(using Context): Mutability =
      def recur(annots: List[Annotation]): Mutability =
        annots match
          case Nil => Mutable
          case annot :: annots =>
            // TODO: multiple mutability annotations?
            annot.getMutability match
              case Some(mut) => mut
              case None => recur(annots)
      if !sym.isClass && sym.owner.isReadonlyClass
        || defn.pureMethods.contains(sym)
      then Readonly
      else recur(sym.annotations)

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
      || sym.isClass && sym.findMutability == Readonly

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


