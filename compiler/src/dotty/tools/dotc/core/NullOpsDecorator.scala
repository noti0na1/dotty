package dotty.tools.dotc
package core

import ast.Trees._
import Contexts._
import Symbols.defn
import Types._

/** Defines operations on nullable types and tree. */
object NullOpsDecorator:

  class StripNullsMap(isDeep: Boolean)(using Context) extends TypeMap:
    def strip(tp: Type): Type =
      val tpw = tp.widenDealias
      val tpws = tpw match {
        case tp @ OrType(lhs, rhs) =>
          val llhs = strip(lhs)
          val rrhs = strip(rhs)
          if rrhs.isNullType then llhs
          else if llhs.isNullType then rrhs
          else derivedOrType(tp, llhs, rrhs)
        case tp @ AndType(tp1, tp2) =>
          // We cannot `tp.derivedAndType(strip(tp1), strip(tp2))` directly,
          // since `stripNull((A | Null) & B)` would produce the wrong
          // result `(A & B) | Null`.
          val tp1s = strip(tp1)
          val tp2s = strip(tp2)
          if (tp1s ne tp1) && (tp2s ne tp2) then
            derivedAndType(tp, tp1s, tp2s)
          else tp
        case tp @ TypeAlias(alias) =>
          derivedAlias(tp, strip(alias))
        case tp @ TypeBounds(lo, hi) =>
          derivedTypeBounds(tp, strip(lo), strip(hi))
        case _ => tp
      }
      if tpws ne tpw then tpws else tp

    override def apply(tp: Type): Type = tp match
      case _ if !isDeep =>
        val r = strip(tp)
        // println(tp)
        // println(tp.widen)
        // println(r)
        r
      case appTp @ AppliedType(tycon, targs) =>
        derivedAppliedType(appTp, tycon, targs.map(this))
      case ptp: PolyType =>
        derivedLambdaType(ptp)(ptp.paramInfos, this(ptp.resType))
      case mtp: MethodType =>
        mapOver(mtp)
      case tp: TypeAlias =>
        mapOver(tp)
      case _ =>
        val tps = strip(tp)
        if tps ne tp then this(tps) else tp
  end StripNullsMap

  extension (self: Type)
    /** Syntactically strips the nullability from this type.
     *  If the type is `T1 | ... | Tn`, and `Ti` references to `Null`,
     *  then return `T1 | ... | Ti-1 | Ti+1 | ... | Tn`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     *  The type will not be changed if explicit-nulls is not enabled.
     */
    def stripNull(using Context): Type = {
      // def strip(tp: Type): Type =
      //   val tpWiden = tp.widenDealias
      //   val tpStripped = tpWiden match {
      //     case tp @ OrType(lhs, rhs) =>
      //       val llhs = strip(lhs)
      //       val rrhs = strip(rhs)
      //       if rrhs.isNullType then llhs
      //       else if llhs.isNullType then rrhs
      //       else tp.derivedOrType(llhs, rrhs)
      //     case tp @ AndType(tp1, tp2) =>
      //       // We cannot `tp.derivedAndType(strip(tp1), strip(tp2))` directly,
      //       // since `stripNull((A | Null) & B)` would produce the wrong
      //       // result `(A & B) | Null`.
      //       val tp1s = strip(tp1)
      //       val tp2s = strip(tp2)
      //       if (tp1s ne tp1) && (tp2s ne tp2) then
      //         tp.derivedAndType(tp1s, tp2s)
      //       else tp
      //     case tp @ TypeBounds(lo, hi) =>
      //       tp.derivedTypeBounds(strip(lo), strip(hi))
      //     case tp => tp
      //   }
      //   if tpStripped ne tpWiden then tpStripped else tp

      // if ctx.explicitNulls then strip(self) else self
      if ctx.explicitNulls then new StripNullsMap(false)(self) else self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(using Context): Boolean = {
      val stripped = self.stripNull
      stripped ne self
    }

    /** Strips nulls from this type deeply.
     *  Compaired to `stripNull`, `stripNullsDeep` will apply `stripNull` to
     *  each member of function types as well.
     */
    def stripNullsDeep(using Context): Type =
      // object DeepStripNullsMap extends TypeMap:
      //   override def apply(tp: Type): Type = tp match
      //     case appTp @ AppliedType(tycon, targs) =>
      //       derivedAppliedType(appTp, tycon, targs.map(this))
      //     case ptp: PolyType =>
      //       derivedLambdaType(ptp)(ptp.paramInfos, this(ptp.resType))
      //     case mtp: MethodType =>
      //       mapOver(mtp)
      //     case tp: TypeAlias =>
      //       mapOver(tp)
      //     case _ =>
      //       val tp1 = tp.stripNull
      //       if tp1 eq tp then tp else this(tp1)
      // end DeepStripNullsMap

      if ctx.explicitNulls then new StripNullsMap(true)(self) else self

  end extension

  import ast.tpd._

  extension (self: Tree)
    // cast the type of the tree to a non-nullable type
    def castToNonNullable(using Context): Tree = self.typeOpt match {
      case OrNull(tp) => self.cast(tp)
      case _ => self
    }
end NullOpsDecorator