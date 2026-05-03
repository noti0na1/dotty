package dotty.tools
package repl

import scala.util.control.NonFatal

/** Per-line transcript writer for the REPL.
 *
 *  When the user passes `-Xrepl-history-file:<path>`, the driver wraps
 *  each line's interpretation in [[captureLine]] which:
 *
 *    1. Tees the REPL's output stream and `System.out`/`System.err`
 *       into a per-line buffer for the duration of the line.
 *    2. After the line completes, appends an entry to the configured
 *       file in transcript form:
 *
 *           scala> <input>
 *           <output>
 *
 *           scala> <next input>
 *           ...
 *
 *  No global mutable state lives in this module; the file IS the
 *  history. A reader (typically an `agent[T]` body that wants to
 *  surface recent REPL context to the LLM) reads it like any other
 *  file:
 *
 *  {{{
 *    val transcript = scala.io.Source.fromFile("history.repl").mkString
 *    agent[String](s"continuation of: $transcript")
 *  }}}
 *
 *  The file is opened append-only so multiple runs against the same
 *  path stack chronologically. ANSI color codes are stripped before
 *  writing. The writer is best-effort: I/O failures during a write
 *  are swallowed silently rather than disturbing the REPL.
 */
object ReplHistory:

  /** Run `work`, capturing everything written to `tee` (the REPL
   *  driver's `out` stream) AND `System.out` / `System.err`, then
   *  append a transcript entry to `historyFile` if the path is
   *  non-empty.
   *
   *  When `historyFile` is empty, work runs untouched (no tee
   *  installation, no system-stream redirection): the no-flag path
   *  has zero overhead.
   */
  private[repl] def captureLine[A](
      tee: TeePrintStream,
      historyFile: String,
      input: String
  )(work: => A): A =
    if historyFile.isEmpty then return work

    val origOut = System.out
    val origErr = System.err
    val buf = new java.io.ByteArrayOutputStream
    val sysTee = teeStream(buf, origOut)
    val redirected =
      try
        System.setOut(sysTee)
        System.setErr(sysTee)
        true
      catch case NonFatal(_) => false
    try
      tee.withCapture(buf)(work)
    finally
      if redirected then
        try System.setOut(origOut) catch case NonFatal(_) => ()
        try System.setErr(origErr) catch case NonFatal(_) => ()
      try sysTee.flush() catch case NonFatal(_) => ()
      append(historyFile, input, buf.toString)

  /** Append one transcript entry to `path`. Skipped silently when both
   *  `input` and `output` are empty (e.g. blank line) or when I/O
   *  fails. Creates the parent directory on demand.
   */
  private def append(path: String, input: String, output: String): Unit =
    val cleanInput = input.stripTrailing
    val cleanOutput = stripAnsi(output).stripTrailing
    if cleanInput.isEmpty && cleanOutput.isEmpty then return
    try
      val file = new java.io.File(path)
      val parent = file.getParentFile
      if parent != null && !parent.exists() then parent.mkdirs()
      val w = new java.io.FileWriter(file, /* append = */ true)
      try
        if cleanInput.nonEmpty then
          w.write("scala> ")
          w.write(cleanInput)
          w.write("\n")
        if cleanOutput.nonEmpty then
          w.write(cleanOutput)
          w.write("\n")
        w.write("\n")
      finally w.close()
    catch case NonFatal(_) => ()

  private def teeStream(
      capture: java.io.OutputStream,
      forward: java.io.PrintStream
  ): java.io.PrintStream =
    val raw = new java.io.OutputStream:
      override def write(b: Int): Unit =
        capture.write(b)
        forward.write(b)
      override def write(b: Array[Byte], off: Int, len: Int): Unit =
        capture.write(b, off, len)
        forward.write(b, off, len)
      override def flush(): Unit = forward.flush()
    new java.io.PrintStream(raw, /* autoFlush = */ true)

  private val ansi = java.util.regex.Pattern.compile("\\[[0-9;]*m")
  private def stripAnsi(s: String): String =
    if s.indexOf('') < 0 then s
    else ansi.matcher(s).replaceAll("")

  /** A `PrintStream` that forwards to a primary destination and, when
   *  a capture is installed, also writes everything to the capture.
   *  Used by the REPL driver as its `out` field so [[captureLine]] can
   *  intercept the rendering without disturbing the caller-supplied
   *  output stream.
   */
  final class TeePrintStream(primary: java.io.PrintStream)
      extends java.io.PrintStream(primary):
    @volatile private var capture: java.io.OutputStream | Null = null

    /** Install `buf` as the capture target for the duration of `work`,
     *  then restore whatever capture was installed before. Reentrant.
     */
    private[repl] def withCapture[A](buf: java.io.OutputStream)(work: => A): A =
      val prev = capture
      capture = buf
      try work
      finally capture = prev

    override def write(b: Int): Unit =
      super.write(b)
      val c = capture
      if c != null then
        try c.write(b) catch case NonFatal(_) => ()

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      super.write(b, off, len)
      val c = capture
      if c != null then
        try c.write(b, off, len) catch case NonFatal(_) => ()
  end TeePrintStream

end ReplHistory
