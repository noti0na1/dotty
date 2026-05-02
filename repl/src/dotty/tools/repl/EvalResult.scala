package dotty.tools
package repl

/** The result of a non-throwing `evalSafe` call: either a successful
 *  value of type `T`, or an [[Eval.CompileFailure]] describing the
 *  compile-time failure of *this call* (not of any nested eval inside
 *  the body — those propagate as exceptions from `get` like any other
 *  body exception). Designed for agent / LLM workflows that want to
 *  feed the error text back into a generator and retry rather than
 *  handle a thrown exception.
 *
 *  Lives in `dotty.tools.repl` so the eval-output classloader routes
 *  it through the parent loader and there's a single shared `Class`
 *  on both sides of the eval / REPL boundary (see EVAL.md
 *  "Classloader bridging"). We deliberately avoid `scala.Either` on
 *  the API surface because Scala-library types resolve to two
 *  distinct `Class` objects across that boundary, which trips the
 *  JVM's loader-constraint check with `LinkageError`.
 *
 *  ```
 *  Eval.evalSafe[Int](code) match
 *    case r if r.isSuccess => use(r.get)
 *    case r                => regenerate(r.error.nn.errors)
 *  ```
 */
final class EvalResult[T] private[repl] (
    private val _isSuccess: Boolean,
    private val _value: AnyRef
):
  def isSuccess: Boolean = _isSuccess
  def isFailure: Boolean = !_isSuccess

  /** The body's return value on success, or throws an
   *  [[EvalCompileException]] constructed from the stored
   *  [[Eval.CompileFailure]] on failure (matches the throwing
   *  `eval[T]` form's behaviour).
   */
  def get: T =
    if _isSuccess then _value.asInstanceOf[T]
    else
      val f = _value.asInstanceOf[Eval.CompileFailure]
      throw new EvalCompileException(f.errors, f.source)

  /** The compile-time failure on a failed result, or `null` on
   *  success. The returned [[Eval.CompileFailure]] carries the
   *  diagnostic strings (`errors`) and the synthesised source the
   *  eval driver was compiling (`source`).
   */
  def error: Eval.CompileFailure | Null =
    if _isSuccess then null
    else _value.asInstanceOf[Eval.CompileFailure]

  /** The body's return value on success, or `default` on failure. */
  def getOrElse(default: T): T =
    if _isSuccess then _value.asInstanceOf[T] else default

  override def toString: String =
    if _isSuccess then s"EvalResult.Success(${_value})"
    else
      val f = _value.asInstanceOf[Eval.CompileFailure]
      s"EvalResult.Failure(${f.errors.length} error(s))"

object EvalResult:
  def success[T](value: T): EvalResult[T] =
    new EvalResult(true, value.asInstanceOf[AnyRef])

  def failure[T](error: Eval.CompileFailure): EvalResult[T] =
    new EvalResult(false, error)
