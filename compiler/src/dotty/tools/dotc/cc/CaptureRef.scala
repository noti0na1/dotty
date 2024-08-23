package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Decorators.*
import util.{SimpleIdentitySet, Property}
import typer.ErrorReporting.Addenda
import TypeComparer.subsumesExistentially
import util.common.alwaysTrue
import scala.collection.mutable
import CCState.*
import Periods.NoRunId
import compiletime.uninitialized
import StdNames.nme

/** A trait for references in CaptureSets. These can be NamedTypes, ThisTypes or ParamRefs,
 *  as well as two kinds of AnnotatedTypes representing reach and maybe capabilities.
 */
trait CaptureRef extends TypeProxy, ValueType:
  private var myCaptureSet: CaptureSet | Null = uninitialized
  private var myCaptureSetRunId: Int = NoRunId
  private var mySingletonCaptureSet: CaptureSet.Const | Null = null

  /** Is the reference tracked? This is true if it can be tracked and the capture
   *  set of the underlying type is not always empty.
   */
  final def isTracked(using Context): Boolean =
    this.isTrackableRef && (isMaxCapability || !captureSetOfInfo.isAlwaysEmpty)

  /** Is this a reach reference of the form `x*`? */
  final def isReach(using Context): Boolean = this match
    case AnnotatedType(_, annot) => annot.symbol == defn.ReachCapabilityAnnot
    case _ => false

  /** Is this a maybe reference of the form `x?`? */
  final def isMaybe(using Context): Boolean = this match
    case AnnotatedType(_, annot) => annot.symbol == defn.MaybeCapabilityAnnot
    case _ => false

  final def stripReach(using Context): CaptureRef =
    if isReach then
      val AnnotatedType(parent: CaptureRef, _) = this: @unchecked
      parent
    else this

  final def stripMaybe(using Context): CaptureRef =
    if isMaybe then
      val AnnotatedType(parent: CaptureRef, _) = this: @unchecked
      parent
    else this

  /** Is this reference the generic root capability `cap` ? */
  final def isRootCapability(using Context): Boolean = this match
    case tp: TermRef => tp.name == nme.CAPTURE_ROOT && tp.symbol == defn.captureRoot
    case _ => false

  /** Is this reference capability that does not derive from another capability ? */
  final def isMaxCapability(using Context): Boolean = this match
    case tp: TermRef => tp.isRootCapability || tp.info.derivesFrom(defn.Caps_Exists)
    case tp: TermParamRef => tp.underlying.derivesFrom(defn.Caps_Exists)
    case _ => false

  /** Normalize reference so that it can be compared with `eq` for equality */
  final def normalizedRef(using Context): CaptureRef = this match
    case tp @ AnnotatedType(parent: CaptureRef, annot) if tp.isTrackableRef =>
      tp.derivedAnnotatedType(parent.normalizedRef, annot)
    case tp: TermRef if tp.isTrackableRef =>
      // tp.symbol.termRef
      this
    case _ => this

  /** The capture set consisting of exactly this reference */
  final def singletonCaptureSet(using Context): CaptureSet.Const =
    if mySingletonCaptureSet == null then
      mySingletonCaptureSet = CaptureSet(this.normalizedRef)
    mySingletonCaptureSet.uncheckedNN

  /** The capture set of the type underlying this reference */
  final def captureSetOfInfo(using Context): CaptureSet =
    if ctx.runId == myCaptureSetRunId then myCaptureSet.nn
    else if myCaptureSet.asInstanceOf[AnyRef] eq CaptureSet.Pending then CaptureSet.empty
    else
      myCaptureSet = CaptureSet.Pending
      val computed = CaptureSet.ofInfo(this)
      if !isCaptureChecking || underlying.isProvisional then
        myCaptureSet = null
      else
        myCaptureSet = computed
        myCaptureSetRunId = ctx.runId
      computed

  final def invalidateCaches() =
    myCaptureSetRunId = NoRunId

  /** x subsumes x
   *   this subsumes this.f
   *   x subsumes y  ==>  x* subsumes y, x subsumes y?
   *   x subsumes y  ==>  x* subsumes y*, x? subsumes y?
   *   x: x1.type /\ x1 subsumes y  ==>  x subsumes y
   */
  final def subsumes(y: CaptureRef)(using Context): Boolean =
    def compareCaptureRefs(x: Type, y: Type): Boolean =
      (x eq y)
      || y.match
          case y: CaptureRef => x.match
            case x: CaptureRef => x.subsumes(y)
            case _ => false
          case _ => false

    def compareUndelying(x: Type): Boolean = x match
      case x: SingletonCaptureRef => x.subsumes(y)
      case x: AndType => compareUndelying(x.tp1) || compareUndelying(x.tp2)
      case x: OrType => compareUndelying(x.tp1) && compareUndelying(x.tp2)
      case _ => false

    if (this eq y) || this.isRootCapability then return true

    // similar to compareNamed in TypeComparer
    y match
      case y: TermRef =>
        this match
          case x: TermRef =>
            val xSym = x.symbol
            val ySym = y.symbol

            // check x.f and y.f
            if (xSym ne NoSymbol)
                && (xSym eq ySym)
                && compareCaptureRefs(x.prefix, y.prefix)
              || (x.name eq y.name)
                && x.isPrefixDependentMemberRef
                && compareCaptureRefs(x.prefix, y.prefix)
                && x.signature == y.signature
                && !(xSym.isClass && ySym.isClass)
            then return true
          case _ =>

        // shorten
        if compareCaptureRefs(this, y.prefix) then return true
        // underlying
        if compareCaptureRefs(this, y.info) then return true
      case MaybeCapability(y1) => return this.stripMaybe.subsumes(y1)
      case _ =>

    return this.match
      case ReachCapability(x1) => x1.subsumes(y.stripReach)
      case x: TermRef => compareUndelying(x.info)
      case CapturingType(x1, _) => compareUndelying(x1)
      case x: TermParamRef => subsumesExistentially(x, y)
      case x: TypeRef => assumedContainsOf(x).contains(y)
      case _ => false

  def assumedContainsOf(x: TypeRef)(using Context): SimpleIdentitySet[CaptureRef] =
    CaptureSet.assumedContains.getOrElse(x, SimpleIdentitySet.empty)

end CaptureRef

trait SingletonCaptureRef extends SingletonType, CaptureRef

