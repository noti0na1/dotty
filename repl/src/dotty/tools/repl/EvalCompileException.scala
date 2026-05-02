package dotty.tools
package repl

/** Thrown by `Eval.evalIsolated` when the synthesised wrapper module
 *  fails to compile inside the standalone eval driver. This is the
 *  most common user-visible failure mode for `eval(...)`: an unknown
 *  identifier in the body, a type mismatch when `eval[T]` pins the
 *  return type, a parse error, etc.
 *
 *  The exception carries the diagnostic messages and the generated
 *  wrapper source as structured fields so callers can pattern-match
 *  or inspect them programmatically rather than having to parse the
 *  human-readable `getMessage` output.
 *
 *  `errors` is `Array[String]` (not `Seq[String]`) so the type stays
 *  on the JVM-intrinsic API surface and crosses the eval / REPL
 *  classloader boundary cleanly. A Scala-collection type would
 *  resolve to two distinct `Class` objects across the boundary and
 *  produce a `LinkageError` when a user `catch`-clause touches the
 *  field.
 *
 *  Errors thrown by the body *at runtime* (e.g. `eval("1 / 0")`
 *  raising `ArithmeticException`) propagate as the body's own
 *  exception, not as `EvalCompileException`.
 */
final class EvalCompileException(
    val errors: Array[String],
    val generatedSource: String
) extends RuntimeException(EvalCompileException.formatMessage(errors, generatedSource))

object EvalCompileException:
  private def formatMessage(errors: Array[String], generatedSource: String): String =
    val joined =
      val sb = new StringBuilder
      var i = 0
      while i < errors.length do
        if i > 0 then sb.append('\n')
        sb.append(errors(i))
        i += 1
      sb.toString
    s"eval failed to compile:\n$joined\n\nGenerated source:\n$generatedSource"
