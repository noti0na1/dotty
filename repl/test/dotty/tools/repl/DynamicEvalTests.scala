package dotty.tools
package repl

import org.junit.Test
import org.junit.Assert._

private object DynamicEvalAssertions:
  def assertContains(needle: String, haystack: String): Unit =
    assertTrue(s"expected to contain `$needle`, got:\n$haystack", haystack.contains(needle))
import DynamicEvalAssertions.*

/** Tests for dynamic `eval[T](code: String): T`.
 *
 *  Behaviour summary:
 *    - At runtime, `eval` calls back into the REPL driver, which spins
 *      up a separate dotc Driver to compile + run `code` against the
 *      live REPL session's classpath.
 *    - The argument can be any `String`: literal, `val`, `s"..."`, etc.
 *    - The REPL parser-stage rewriter injects `Eval.bind("z", z)` for
 *      every lambda parameter (and block-local `val`) syntactically in
 *      scope at the call site, so `xs.map(z => eval[Int]("z + 1"))` Just
 *      Works.
 *    - REPL session state (vals, vars, defs, classes, givens) is
 *      imported into the body's scope.
 *    - Return type is the polymorphic `T`; the user ascribes the
 *      expected type and a runtime cast bridges to it. A mismatch
 *      surfaces as `ClassCastException`.
 *
 *  Each section below targets one axis of the behaviour.
 */
class DynamicEvalTests extends ReplTest:

  // ===========================================================================
  // 1. Basics: primitive return types via the polymorphic T parameter.
  // ===========================================================================

  @Test def returnsInt = initially {
    run("""val r: Int = eval("1 + 2")""")
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def returnsLong = initially {
    run("""val r: Long = eval("1000000000000L")""")
    assertContains("val r: Long = 1000000000000L", storedOutput())
  }

  @Test def returnsDouble = initially {
    run("""val r: Double = eval("math.sqrt(16.0)")""")
    assertContains("val r: Double = 4.0", storedOutput())
  }

  @Test def returnsBoolean = initially {
    run("""val r: Boolean = eval("1 < 2 && 3 != 4")""")
    assertContains("val r: Boolean = true", storedOutput())
  }

  @Test def returnsChar = initially {
    run("""val r: Char = eval("'a'.toUpper")""")
    assertContains("val r: Char = 'A'", storedOutput())
  }

  @Test def returnsString = initially {
    run("""val r: String = eval("\"hello\".reverse")""")
    assertContains("""val r: String = "olleh"""", storedOutput())
  }

  @Test def returnsTuple = initially {
    run("""val r: (Int, String) = eval("(42, \"answer\")")""")
    assertContains("""val r: (Int, String) = (42, "answer")""", storedOutput())
  }

  @Test def returnsOption = initially {
    run("""val r: Option[Int] = eval("Some(7)")""")
    assertContains("val r: Option[Int] = Some(7)", storedOutput())
  }

  @Test def returnsList = initially {
    run("""val r: List[Int] = eval("List(1, 2, 3)")""")
    assertContains("val r: List[Int] = List(1, 2, 3)", storedOutput())
  }

  // ===========================================================================
  // 2. Dynamic strings: the argument can be any String value, not just a literal.
  // ===========================================================================

  @Test def evalsStringVariable = initially {
    run("""|val op: String = "1 + 2"
           |val r: Int = eval(op)""".stripMargin)
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def evalsInterpolatedString = initially {
    run("""|val n: Int = 5
           |val expr: String = s"$n * $n + 1"
           |val r: Int = eval(expr)""".stripMargin)
    assertContains("val r: Int = 26", storedOutput())
  }

  @Test def evalsDifferentStringsPerIteration = initially {
    run("""val r: List[Int] = (1 to 3).toList.map(i => eval[Int](s"$i * 10"))""")
    assertContains("List(10, 20, 30)", storedOutput())
  }

  @Test def dynamicDispatchOverFunctionNames = initially {
    // Dispatching on a name held in a String. `f` and `g` live at
    // REPL session level, so they're reachable inside the eval body
    // via the auto-injected `import rs$line$N.*`. The lambda param
    // `op` is captured by the rewriter (as a String) but only used
    // for the s-interpolation that builds the body string; the body
    // itself never references `op` and instead calls the named
    // session-level function directly.
    run("""|def f(i: Int) = i * 2
           |def g(i: Int) = i - 2
           |val ops = List("f", "g")
           |val r: List[Int] = ops.map(op => eval[Int](s"$op(2)"))""".stripMargin)
    assertContains("val r: List[Int] = List(4, 0)", storedOutput())
  }

  // ===========================================================================
  // 3. Polymorphic return type T: eval composes at any expression position.
  // ===========================================================================

  @Test def ascribesAtAssignmentPosition = initially {
    run("""val r: Int = eval("21 * 2")""")
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def ascribesInsideExpression = initially {
    run("""val r: Int = eval[Int]("4") + eval[Int]("5")""")
    assertContains("val r: Int = 9", storedOutput())
  }

  @Test def wrongAscriptionFailsAtRuntime = initially {
    // Without an explicit `[T]` the typer can't propagate `Int` from
    // the val ascription back through eval's overload resolution, so
    // T defaults to Nothing and the post-typer phase falls back to
    // `Any` as the wrapper return type. The mismatch surfaces only
    // at the call-site cast.
    run("""val r: Int = eval("\"not an int\"")""")
    assertContains("ClassCastException", storedOutput())
  }

  @Test def explicitTypeArgFailsAtCompile = initially {
    // With an explicit `[Int]` the post-typer phase pins the wrapper's
    // return type, so a mismatch becomes a compile error from the
    // eval driver before any code runs.
    run("""val r: Int = eval[Int]("false")""")
    val out = storedOutput()
    assertContains("eval failed to compile", out)
    assertContains("Boolean", out)
    assertContains("Required: Int", out)
  }

  @Test def explicitTypeArgStringNotInt = initially {
    run("""val r: Int = eval[Int]("\"hi\"")""")
    val out = storedOutput()
    assertContains("eval failed to compile", out)
    assertContains("Required: Int", out)
  }

  @Test def explicitTypeArgGenericMismatch = initially {
    // Body returns `List[String]`, the type arg pins `List[Int]`;
    // each element fails individually.
    run("""val r: List[Int] = eval[List[Int]]("List(\"a\", \"b\")")""")
    val out = storedOutput()
    assertContains("eval failed to compile", out)
    assertContains("Required: Int", out)
  }

  @Test def explicitGenericTypeArgPasses = initially {
    run("""val r: List[Int] = eval[List[Int]]("List(1, 2, 3)")""")
    assertContains("val r: List[Int] = List(1, 2, 3)", storedOutput())
  }

  // ===========================================================================
  // 4. Side effects: bodies run for their effects, not just their value.
  // ===========================================================================

  @Test def runsSideEffect = initially {
    run("""eval("println(\"hello-side-effect\")")""")
    assertContains("hello-side-effect", storedOutput())
  }

  @Test def sideEffectsRunInOrder = initially {
    run("""eval("println(\"a\"); println(\"b\"); println(\"c\")")""")
    val out = storedOutput()
    val ai = out.indexOf("a")
    val bi = out.indexOf("b")
    val ci = out.indexOf("c")
    assertTrue("a before b", ai >= 0 && ai < bi)
    assertTrue("b before c", bi >= 0 && bi < ci)
  }

  // ===========================================================================
  // 5. Lambda parameter capture (the headline feature). The parser-stage
  //    rewriter injects bindings for every lambda param syntactically in scope.
  // ===========================================================================

  @Test def lambdaParamCapturedInLiteralBody = initially {
    run("""val r: List[Int] = List(1, 2, 3).map(z => eval[Int]("z + 1"))""")
    assertContains("List(2, 3, 4)", storedOutput())
  }

  @Test def lambdaParamCapturedInDynamicBody = initially {
    run("""|val op: String = "z * 2"
           |val r: List[Int] = List(10, 20, 30).map(z => eval[Int](op))""".stripMargin)
    assertContains("List(20, 40, 60)", storedOutput())
  }

  @Test def lambdaParamWithSideEffect = initially {
    run("""val r: List[Int] = List(1, 2, 3).map(z => eval[Int]("println(z); z * 10"))""")
    val out = storedOutput()
    assertContains("List(10, 20, 30)", out)
    assertContains("1\n", out)
    assertContains("2\n", out)
    assertContains("3\n", out)
  }

  @Test def nestedLambdasBothCaptured = initially {
    run("""val r: List[Int] = List(1, 2).flatMap(a => List(10, 20).map(b => eval[Int]("a * 100 + b")))""")
    assertContains("List(110, 120, 210, 220)", storedOutput())
  }

  @Test def innerLambdaShadowsOuter = initially {
    // Inner `z` shadows outer `z`; the body's `z` resolves to the inner one,
    // matching normal Scala lexical scoping.
    run("""val r: List[Int] = List(1, 2).flatMap(z => List(100, 200).map(z => eval[Int]("z + 1")))""")
    assertContains("List(101, 201, 101, 201)", storedOutput())
  }

  @Test def blockLocalValShadowsLambdaParam = initially {
    // The lambda binds `x`, the block-local `val x` shadows it. The
    // rewriter's `currentBindings` deduplicates innermost-first, so
    // only the inner `x` is captured; the body sees the inner.
    run(
      """|val r: List[Int] = List(1, 2, 3).map { x =>
         |  val x = 100
         |  eval[Int]("x")
         |}""".stripMargin
    )
    assertContains("List(100, 100, 100)", storedOutput())
  }

  @Test def lambdaParamShadowsMethodParam = initially {
    // `def f(x: Int)` binds `x`; the inner lambda binds another `x`.
    // The eval body picks up the lambda's `x`.
    run(
      """|def f(x: Int): List[Int] =
         |  List(10, 20).map(x => eval[Int]("x + 1"))
         |f(999)""".stripMargin
    )
    assertContains("List(11, 21)", storedOutput())
  }

  @Test def innerVarShadowsOuterVal = initially {
    // The inner block-local `var x` shadows the outer `val x`. The
    // rewriter must trigger the var-cell sync-back path for the
    // inner — and *only* the inner — even though an outer immutable
    // `x` is also in scope.
    run(
      """|def f(): Int =
         |  val x: Int = 7  // outer val, immutable; would normally bind by-value
         |  {
         |    var x: Int = 0  // inner var, shadows outer
         |    eval[Unit]("x = x + 5")
         |    x
         |  }
         |f()""".stripMargin
    )
    assertContains("val res0: Int = 5", storedOutput())
  }

  @Test def innerValShadowsOuterVar = initially {
    // Outer `var x` shadowed by inner `val x`. The inner is what the
    // body sees, and since the inner is a val no var-cell sync-back
    // machinery fires.
    run(
      """|def f(): Int =
         |  var x: Int = 100  // outer mutable
         |  {
         |    val x: Int = 7  // inner immutable, shadows outer
         |    eval[Int]("x + 1")
         |  }
         |f()""".stripMargin
    )
    assertContains("val res0: Int = 8", storedOutput())
  }

  @Test def methodParamShadowedByBlockLocalDef = initially {
    // The method parameter `g` is shadowed by a block-local `def g`.
    // The block-local def is captured by eta-expansion; the body's
    // call resolves to it.
    run(
      """|def f(g: Int): Int =
         |  def g(x: Int): Int = x * 10
         |  eval[Int]("g(4)")
         |f(999)""".stripMargin
    )
    assertContains("val res0: Int = 40", storedOutput())
  }

  @Test def lambdaCapturingComplexType = initially {
    // The post-typer `EvalTypeAnnotate` phase records `xs2`'s typer-side
    // type, so the synthesised wrapper parameter is `xs2: List[Int]`
    // (with the Int element preserved). If that phase is disabled the
    // runtime would walk the cons cell up the superclass chain and
    // synthesise the weaker `xs2: List[?]` instead.
    run("""|val xs = List(List(0, 1), List(1, 2))
           |val r: List[Int] = xs.flatMap(xs2 => xs2.map(x => eval[Int]("x + xs2.length")))""".stripMargin)
    assertContains("List(2, 3, 3, 4)", storedOutput())
  }

  @Test def capturedListElementTypePreserved = initially {
    // Demonstrates the EvalTypeAnnotate phase keeping element types
    // visible inside the eval body. With only the runtime fallback
    // `xs.head` would return `?`, which can't be used in arithmetic;
    // this body would fail to compile.
    run("""|val xs: List[Int] = List(10, 20, 30)
           |val r: Int = (1 to 1).toList.map(z => eval[Int]("xs.head + xs.last + z")).head""".stripMargin)
    assertContains("val r: Int = 41", storedOutput())
  }

  @Test def capturedMapKeyValueTypesPreserved = initially {
    // Same idea for `Map[String, Int]`. Without typed annotation the
    // body would see `m: Map[?, ?]` and `m("a")` would type as `?`.
    run("""|val m: Map[String, Int] = Map("a" -> 1, "b" -> 2)
           |val r: Int = (1 to 1).toList.map(z => eval[Int]("m(\"a\") + m(\"b\") + z")).head""".stripMargin)
    assertContains("val r: Int = 4", storedOutput())
  }

  // ===========================================================================
  // 6. Block-local `val` capture: names introduced earlier in the same
  //    block are also injected as bindings.
  // ===========================================================================

  @Test def blockLocalValCaptured = initially {
    run("""|val r: List[Int] = List(1, 2, 3).map { z =>
           |  val doubled = z * 2
           |  eval[Int]("doubled + 1")
           |}""".stripMargin)
    assertContains("List(3, 5, 7)", storedOutput())
  }

  // ===========================================================================
  // 7. REPL session state (previous-line definitions) is visible inside eval.
  //    Implementation: at each call, the runtime adds the REPL's output dir to
  //    the eval driver's classpath and prepends `import rs$line$N.{given, *}`
  //    for each valid wrapper.
  // ===========================================================================

  @Test def seesPreviousLineVal = initially {
    run("val n: Int = 7")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("n * n")""")
    assertContains("val r: Int = 49", storedOutput())
  }

  @Test def seesPreviousLineDef = initially {
    run("def square(n: Int): Int = n * n")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("square(6)")""")
    assertContains("val r: Int = 36", storedOutput())
  }

  @Test def seesPreviousLineGiven = initially {
    run("given Int = 99")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("summon[Int]")""")
    assertContains("val r: Int = 99", storedOutput())
  }

  @Test def seesPreviousLineCaseClass = initially {
    run("case class Pt(x: Int, y: Int)")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("Pt(3, 4).x + Pt(3, 4).y")""")
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def replStateAndLambdaParamCombined = initially {
    run("val factor: Int = 10")
  } andThen {
    storedOutput()
    run("""val r: List[Int] = List(1, 2, 3).map(z => eval[Int]("z * factor"))""")
    assertContains("List(10, 20, 30)", storedOutput())
  }

  @Test def readsPreviousLineVar = initially {
    run("var counter: Int = 100")
  } andThen {
    storedOutput()
    run("""val r: Int = eval("counter + 1")""")
    assertContains("val r: Int = 101", storedOutput())
  }

  @Test def mutatesPreviousLineVar = initially {
    run("""|var counter: Int = 1
           |eval[Unit]("counter = counter + 5")
           |counter""".stripMargin)
    assertContains("Int = 6", storedOutput())
  }

  @Test def mutatesPreviousLineVarSequentially = initially {
    run("""|var log: List[Int] = Nil
           |eval[Unit]("log = 1 :: log")
           |eval[Unit]("log = 2 :: log")
           |eval[Unit]("log = 3 :: log")
           |log""".stripMargin)
    assertContains("List(3, 2, 1)", storedOutput())
  }

  @Test def mutatesVarFieldOnReplObject = initially {
    // `Counter` is a REPL-defined class with a mutable field. The eval
    // body mutates the field through the captured instance reference;
    // the change is visible to subsequent reads outside.
    run("""|class Counter(val name: String):
           |  var n: Int = 0
           |val c = new Counter("c1")
           |eval[Unit]("c.n = c.n + 7; c.n = c.n + 3")
           |c.n""".stripMargin)
    assertContains("Int = 10", storedOutput())
  }

  @Test def mutatesVarFieldFromInsideLambda = initially {
    // The class instance `b` is in REPL scope; the eval body mutates its
    // field. Each lambda iteration runs eval, accumulating into `b.n`.
    run("""|class Box:
           |  var n: Int = 0
           |val b = new Box
           |List(1, 2, 3).foreach(z => eval[Unit]("b.n = b.n + z"))
           |b.n""".stripMargin)
    assertContains("Int = 6", storedOutput())
  }

  @Test def methodParameterCaptured = initially {
    // `i` is a method param of `f`; the rewriter pushes it as a scope
    // around `f`'s rhs so the `eval(...)` inside captures it.
    run("""|def f(i: Int): Int = eval[Int]("i + 1")
           |val r: Int = f(5)""".stripMargin)
    assertContains("val r: Int = 6", storedOutput())
  }

  @Test def methodParameterAndReplValTogether = initially {
    // `i` (method param) and `x` (REPL val) are both visible inside the
    // eval body. `i` arrives via the rewriter's binding, `x` via the
    // imported wrapper.
    run("""|def f(i: Int): Int = eval[Int]("x + i")
           |val x: Int = 2
           |val r: Int = f(3)""".stripMargin)
    assertContains("val r: Int = 5", storedOutput())
  }

  @Test def methodParameterPresentEvenIfReplValMissing = initially {
    // `f` is defined before any `x`. Calling it errors because `x`
    // doesn't exist in the REPL session, but `i` (method param) IS
    // captured, so the failure is specifically "Not found: x" and not
    // "Not found: i".
    run("""|def f(i: Int): Int = eval[Int]("x + i")
           |f(1)""".stripMargin)
    val out = storedOutput()
    assertContains("Not found: x", out)
    assertTrue(s"`i` should not be reported as missing, got:\n$out", !out.contains("Not found: i"))
  }

  @Test def methodParametersInMultipleClauses = initially {
    run("""|def f(a: Int)(b: Int): Int = eval[Int]("a + b * 10")
           |val r: Int = f(3)(4)""".stripMargin)
    assertContains("val r: Int = 43", storedOutput())
  }

  @Test def methodWithMultipleEvalsInBody = initially {
    // The method body has two consecutive eval calls. Both capture `x`
    // (the method's parameter); the first runs for its side effect, the
    // second yields the return value.
    run("""|def f(x: Int): Int =
           |  eval[Unit]("println(x)")
           |  eval[Int]("x + 1")
           |val r: Int = f(5)""".stripMargin)
    val out = storedOutput()
    assertContains("5", out)               // println side effect from the first eval
    assertContains("val r: Int = 6", out)  // value from the second eval
  }

  // ---------------------------------------------------------------------------
  // Block-local def capture. The rewriter eta-expands a captured def
  // into a `FunctionN` lambda at the bind site; the typer infers the
  // function type, the type-annotation phase records it, and the eval
  // body sees the def as a precisely-typed function value.
  // ---------------------------------------------------------------------------

  @Test def blockLocalDefCapturedUnary = initially {
    run("""|def f(i: Int): Int =
           |  def g(j: Int) = i * j
           |  eval[Int]("g(2)")
           |val r: Int = f(7)""".stripMargin)
    assertContains("val r: Int = 14", storedOutput())
  }

  @Test def blockLocalDefCapturedMultiArg = initially {
    run("""|def f(): Int =
           |  def g(a: Int, b: Int) = a * 100 + b
           |  eval[Int]("g(3, 4)")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 304", storedOutput())
  }

  @Test def blockLocalDefCapturedNullary = initially {
    run("""|def f(): Int =
           |  def g = 42
           |  eval[Int]("g()")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def blockLocalDefAndValTogether = initially {
    run("""|def f(i: Int): Int =
           |  val k = 100
           |  def g(j: Int) = i + j
           |  eval[Int]("g(5) + k")
           |val r: Int = f(2)""".stripMargin)
    assertContains("val r: Int = 107", storedOutput())
  }

  @Test def blockLocalDefClosesOverEnclosing = initially {
    // The def captures `i` from `f`'s scope. The eta-expansion the
    // rewriter generates is just `(j) => g(j)`, which itself is a
    // closure over `g`, which is itself a closure over `i`. So `i`
    // doesn't need to flow into the eval body explicitly; it's
    // already inside the captured function value.
    run("""|def f(i: Int): Int =
           |  def doubleIt(j: Int) = i * 2 + j
           |  eval[Int]("doubleIt(0)")
           |val r: Int = f(11)""".stripMargin)
    assertContains("val r: Int = 22", storedOutput())
  }

  @Test def blockLocalDefInsideLambda = initially {
    run("""|val r: List[Int] = List(1, 2, 3).map { z =>
           |  def square(x: Int) = x * x
           |  eval[Int]("square(z)")
           |}""".stripMargin)
    assertContains("List(1, 4, 9)", storedOutput())
  }

  @Test def genericBlockLocalDefCaptured = initially {
    // Generic local defs are captured by polymorphic eta-expansion:
    // `[T] => (x: T) => g[T](x)`. The eval body picks the type
    // argument explicitly via `g[Int](...)`.
    run("""|def f(): Int =
           |  def g[T](x: T) = x
           |  eval[Int]("g[Int](42)")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 42", storedOutput())
  }

  @Test def genericBlockLocalDefMultipleTypeParams = initially {
    run("""|def f(): String =
           |  def g[A, B](a: A, b: B): (B, A) = (b, a)
           |  eval[(String, Int)]("g[Int, String](1, \"hi\")")._1
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "hi"""", storedOutput())
  }

  @Test def genericBlockLocalDefBoundedTypeParam = initially {
    // A type bound on the local def survives the eta-expansion
    // (`[T <: AnyRef] => (x: T) => g[T](x)`). Calling it with a
    // value type that doesn't satisfy the bound fails to compile.
    run("""|def f(): String =
           |  def g[T <: AnyRef](x: T): T = x
           |  eval[String]("g[String](\"hi\")")
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "hi"""", storedOutput())
  }

  @Test def genericBlockLocalDefWithReplValueCaptured = initially {
    // Mixed capture: `factor` (a val) and `g` (a generic def) both
    // captured at the same eval site.
    run("""|def f(): Int =
           |  val factor = 10
           |  def g[T](x: T): T = x
           |  eval[Int]("g[Int](7) * factor")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 70", storedOutput())
  }

  @Test def contextBoundDefSkipped = initially {
    // Context bounds desugar to a separate `using` clause, which the
    // multi-paramlist guard rejects. Eta-expansion is therefore
    // skipped and the eval body falls back to the legacy "Not found"
    // failure mode.
    run("""|def f(): Int =
           |  def g[T : Numeric](x: T): T = summon[Numeric[T]].plus(x, x)
           |  eval[Int]("g[Int](21)")
           |f()""".stripMargin)
    val out = storedOutput()
    assertContains("Not found: g", out)
  }

  // ---------------------------------------------------------------------------
  // Block-local `given` capture. Givens declared in an enclosing
  // method or block are captured into the eval wrapper's `using` clause
  // so `summon[T]` inside the body resolves against them. Named givens
  // are also reachable by name. Anonymous givens (`given Int = 99`)
  // capture via `summon[<tpt>]` at the f-scope, so any dependency on
  // other givens is resolved before the value is handed to eval.
  // ---------------------------------------------------------------------------

  @Test def anonymousLocalGivenSummonable = initially {
    run("""|def f(): Int =
           |  given Int = 99
           |  eval[Int]("summon[Int]")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 99", storedOutput())
  }

  @Test def namedLocalGivenSummonable = initially {
    run("""|def f(): Int =
           |  given x: Int = 7
           |  eval[Int]("summon[Int]")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def namedLocalGivenReachableByName = initially {
    run("""|def f(): Int =
           |  given x: Int = 7
           |  eval[Int]("x + 1")
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 8", storedOutput())
  }

  @Test def localOrderingGivenSummoned = initially {
    // The captured Ordering[Int] is a non-trivial value (not a primitive).
    // Verifies that the using clause carries through reference types.
    run("""|def f(): String =
           |  given Ordering[Int] = Ordering.Int.reverse
           |  eval[String]("List(3, 1, 4, 1, 5).sorted.mkString(\",\")")
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "5,4,3,1,1"""", storedOutput())
  }

  @Test def dependentGivensResolvedAtCaptureSite = initially {
    // The second given depends on the first via `summon[Int]` in its
    // RHS. Resolution happens at the f-scope where both givens are in
    // scope, so the captured `str` value is "7!" and the eval body's
    // `summon[String]` returns it.
    run("""|def f(): String =
           |  given Int = 7
           |  given str: String = summon[Int].toString + "!"
           |  eval[String]("summon[String]")
           |val r: String = f()""".stripMargin)
    assertContains("""val r: String = "7!"""", storedOutput())
  }

  @Test def genericLocalGivenSilentlySkipped = initially {
    // Generic local givens have an empty parser-stage name and are
    // not yet captured. The eval body should still compile; it just
    // resolves against whatever non-local Ordering is available.
    // This test asserts there's no crash and the eval body produces
    // *some* sorted output (the default lexicographic Ordering for
    // Lists, not the by-length one the local given would have given).
    run("""|def f(): String =
           |  given [T] => Ordering[List[T]] = (a, b) => a.length - b.length
           |  eval[String]("List(List(1,2), List(3), List(4,5,6)).sorted.toString")
           |val r: String = f()""".stripMargin)
    val out = storedOutput()
    assertContains("val r: String =", out)
  }

  // ---------------------------------------------------------------------------
  // Function-local `var`: the eval body captures it by value.
  // ---------------------------------------------------------------------------

  @Test def functionLocalVarReadByEval = initially {
    // A function-local `var` declared in the enclosing block is captured
    // by the rewriter (it's a `ValDef` in scope at the eval call site).
    // The eval body reads it via the captured snapshot.
    run("""|def f(): Int = {
           |  var x = 10
           |  eval[Int]("x + 5")
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def functionLocalVarMutationFromBodyPropagates = initially {
    // The rewriter wraps captured `var`s in `Eval.VarCell`s, declares a
    // local var in the eval body initialised from `cell.value`, runs
    // the body, then writes the local back to the cell. After eval
    // returns the call site reads `cell.value` into the outer var, so
    // mutation propagates.
    run("""|def f(): Int = {
           |  var x = 10
           |  eval[Unit]("x = x + 5")
           |  x
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def functionLocalVarMultipleMutations = initially {
    run("""|def f(): Int = {
           |  var x = 0
           |  eval[Unit]("x = x + 1; x = x * 10; x = x + 3")
           |  x
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 13", storedOutput())
  }

  @Test def functionLocalMultipleVarsMutated = initially {
    run("""|def f(): (Int, String) = {
           |  var x = 1
           |  var s = "hi"
           |  eval[Unit]("x = x + 100; s = s + \"!\"")
           |  (x, s)
           |}
           |val r: (Int, String) = f()""".stripMargin)
    assertContains("""val r: (Int, String) = (101, "hi!")""", storedOutput())
  }

  @Test def lambdaLocalVarMutationPropagates = initially {
    // Same mechanism inside a lambda: block-local vars in the lambda
    // body get cell-wrapped and synced back.
    run("""val r: List[Int] = List(1, 2, 3).map(z => {
          |  var acc = 0
          |  eval[Unit]("acc = z * 100")
          |  acc
          |})""".stripMargin)
    assertContains("List(100, 200, 300)", storedOutput())
  }

  @Test def functionLocalVarMutationViaSyncBack = initially {
    // Working pattern: have eval RETURN the new value, then assign it
    // back to the outer `var` explicitly.
    run("""|def f(): Int = {
           |  var x = 10
           |  x = eval[Int]("x + 5")
           |  x
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 15", storedOutput())
  }

  @Test def functionLocalVarMutationViaClassField = initially {
    // Preferred pattern when the eval body needs to perform multiple
    // writes: hold the mutable state in a class field. The instance
    // reference is captured, and field writes through it propagate.
    run("""|class Cell:
           |  var n: Int = 0
           |def f(): Int = {
           |  val c = new Cell
           |  c.n = 10
           |  eval[Unit]("c.n = c.n + 5")
           |  c.n
           |}
           |val r: Int = f()""".stripMargin)
    assertContains("val r: Int = 15", storedOutput())
  }

  // ---------------------------------------------------------------------------
  // Type parameters of an enclosing method.
  //
  // Term parameters are captured by the rewriter and arrive at the eval body
  // as method parameters whose declared type is recovered from the runtime
  // class of the value. Type parameters themselves are *not* in scope inside
  // the eval body: they have no runtime representation, and the body is
  // compiled in a fresh wrapper module that has no notion of the caller's
  // type variables. But because T is erased on the JVM:
  //   * a body that uses `x: T` at runtime sees the erased value, so methods
  //     available on the runtime class (or `Any`) work;
  //   * `eval[T]("...")` at the call site casts the result to T, so a body
  //     can return its parameter and the value flows through.
  // ---------------------------------------------------------------------------

  @Test def genericMethodEvalUsesParam = initially {
    run("""|def f[T](x: T): String = eval[String]("x.toString")
           |val a: String = f(42)
           |val b: String = f("hi")""".stripMargin)
    val out = storedOutput()
    assertContains("""val a: String = "42"""", out)
    assertContains("""val b: String = "hi"""", out)
  }

  @Test def genericMethodEvalReturnsT = initially {
    // T is erased, so `eval[T]("x")` is essentially `x` cast to T at runtime.
    run("""|def myId[T](x: T): T = eval[T]("x")
           |val a: Int = myId(42)
           |val b: String = myId("hi")""".stripMargin)
    val out = storedOutput()
    assertContains("val a: Int = 42", out)
    assertContains("""val b: String = "hi"""", out)
  }

  // ---------------------------------------------------------------------------
  // Type parameters of an enclosing method, copied onto the wrapper.
  //
  // The rewriter records every enclosing DefDef's type-param clause and emits
  // it onto the wrapper's `def __run__[T, U <: AnyRef, ...]` signature.
  // Erasure means no type argument needs to flow through `Method.invoke`; the
  // body just needs `T` to be a name in scope so it type-checks. Bindings whose
  // types mention these type params (`xs: List[T]`) are rendered as-is.
  // Anchor-aware shadowing detection keeps an outer DefDef's `T` from silently
  // binding to an inner same-named one in the wrapper.
  // ---------------------------------------------------------------------------

  @Test def typeParameterNameInScopeInsideEval = initially {
    // The rewriter copies the enclosing DefDef's type-param clause
    // onto the wrapper's `def __run__[T]` signature, so a body that
    // refers to `T` literally (`val tag: T = x; tag`) type-checks and
    // runs. Erasure means no type argument needs to be passed at the
    // reflective invoke; the body's `T` is just the wrapper's own
    // type parameter, with the same erasure as the caller's `T`.
    run("""|def f[T](x: T): T = eval[T]("val tag: T = x; tag")
           |f(42)""".stripMargin)
    assertContains("val res0: Int = 42", storedOutput())
  }

  @Test def typeParameterUsedInBindingTypeAndBody = initially {
    // The motivating example: a binding whose type mentions T plus a
    // body that uses T explicitly. Both the wrapper's signature and
    // the body need T in scope.
    run("""|def id[A](x: A): A = x
           |def f[T](xs: List[T]): List[T] =
           |  eval[List[T]]("xs.map[T](x => id[T](x))")
           |f(List(1, 2, 3))
           |f(List("a", "b"))""".stripMargin)
    val out = storedOutput()
    assertContains("List(1, 2, 3)", out)
    assertContains("""List("a", "b")""", out)
  }

  @Test def boundedTypeParameterPreserved = initially {
    // Bounded type params (`T <: AnyRef`) should round-trip through
    // the wrapper signature with their bounds intact.
    run("""|def f[T <: AnyRef](x: T): T = eval[T]("x")
           |f("hello")""".stripMargin)
    assertContains("val res0: String = \"hello\"", storedOutput())
  }

  @Test def shadowedTypeParameterIsConservative = initially {
    // When an inner `def g[T]` shadows an outer `def f[T]`, a
    // binding (`xs`) whose type uses the *outer* `T` must not be
    // rendered with the inner's `T` in the wrapper signature: the
    // two are different symbols even though they share a name. Our
    // anchor-aware mentionsLocallyScopedSymbol bails on the binding
    // type, so `xs` falls back to its runtime `Class` (here `List`).
    // Erasure makes the result correct at runtime.
    run("""|def f[T](x: T) =
           |  val xs: List[T] = List(x)
           |  def g[T](y: T) = eval[Any]("(xs, y)")
           |  g[Int](100)
           |val r = f("hello")""".stripMargin)
    assertContains("(List(\"hello\"), 100)", storedOutput())
  }

  // ---------------------------------------------------------------------------
  // Local type aliases.
  //
  // Aliases declared inside a method scope (`type MyInt = Int`) are dealiased
  // before the binding type is rendered: the wrapper module isn't lexically
  // inside the method, so `MyInt` wouldn't resolve there. Session-level
  // aliases are kept as-is — they're nameable through the rewriter's
  // `import rs$line$N.*` bridge.
  // ---------------------------------------------------------------------------

  @Test def localTypeAliasDealiased = initially {
    // A type alias defined inside the method scope can't be named
    // inside the wrapper module (its symbol is term-owned). The
    // post-typer phase dealiases such aliases when rendering the
    // binding type, so the wrapper sees the underlying type.
    run("""|def f(): Int =
           |  type MyInt = Int
           |  val n: MyInt = 42
           |  eval[Int]("n + 1")
           |f()""".stripMargin)
    assertContains("val res0: Int = 43", storedOutput())
  }

  // ---------------------------------------------------------------------------
  // Class-scope capture: eval inside a class method.
  //
  // The rewriter handles a Template (class body) by pushing the class's type
  // parameters and val/var members onto the scope/typeParam stacks. Each
  // member is captured under both its bare name (read-only val) and a
  // `<name>__field` shadow-safe binding (cell-backed for var members). A
  // synthetic `__this__` binding holds the outer instance; the runtime body
  // rewrite step rewrites `this.<x>` → `<x>__field` (or → `__this__.<x>`
  // for non-member selections like method calls).
  // ---------------------------------------------------------------------------

  @Test def classScopeGetterAndSetter = initially {
    // The motivating example: an `eval` inside a class method that
    // reads/writes a private var member, parameterised by a class
    // type-param `T`. The rewriter:
    //   - copies `[T]` from the class onto the wrapper signature,
    //   - captures `v` as both a bare-name binding and a
    //     `v__field` cell-backed binding pointing at `this.v`,
    //   - captures `__this__` for non-member `this.x` accesses,
    //   - rewrites `this.v` in the body to `v__field` so writes
    //     flow through the var-cell sync-back back to `this.v`.
    run(
      """|class C[T](private var v: T):
         |  def get: T = eval[T]("v")
         |  def set(v: T): T = eval[T]("val old = this.v; this.v = v; old")
         |val c = new C[Int](10)
         |c.get
         |c.set(99)
         |c.get""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 10", out)
    assertContains("val res1: Int = 10", out)
    assertContains("val res2: Int = 99", out)
  }

  @Test def classScopeReadOnlyVal = initially {
    // Plain `val` member accessed via the bare name (no shadowing).
    run(
      """|class P(val x: Int, val y: Int):
         |  def magnitudeSquared: Int = eval[Int]("x * x + y * y")
         |val p = new P(3, 4)
         |p.magnitudeSquared""".stripMargin
    )
    assertContains("val res0: Int = 25", storedOutput())
  }

  // ---------------------------------------------------------------------------
  // Multi-line eval bodies.
  //
  // The body is spliced inside the wrapper's `def __run__ = { ... }` braces,
  // so Scala 3 parses it under the brace-based syntax (semicolons / newlines
  // separate statements; indentation isn't structurally significant at the
  // top level). The body can still use indentation-sensitive constructs
  // (`if then ... else`, function literals) inside, as long as they're
  // self-consistent. When the body goes through `rewriteThisInBody` (class
  // scope) it round-trips through the parser + pretty-printer; the tests
  // sidestep `s"..."` interpolations inside such bodies because those don't
  // round-trip cleanly (per EVAL.md "Pretty-printer round-trip in nested
  // eval", same caveat).
  // ---------------------------------------------------------------------------

  @Test def multiLineBodyTripleQuoted = initially {
    // The body is a triple-quoted multi-line string. The wrapper
    // splices it inside `def __run__ = { ... }` braces, so Scala 3
    // parses it under the brace-based syntax (semicolons / newlines
    // separate statements, indentation isn't structurally
    // significant). The body can still use indentation-sensitive
    // constructs (`if then ... else`) inside, as long as they're
    // self-consistent.
    val body =
      "\n  val sum = a + b" +
      "\n  val prod = a * b" +
      "\n  sum + prod\n"
    run(
      "def f(a: Int, b: Int): Int = eval[Int](\"\"\"" + body + "\"\"\")\n" +
      "f(3, 4)"
    )
    assertContains("val res0: Int = 19", storedOutput())
  }

  @Test def multiLineBodyEscapedNewlines = initially {
    // Same multi-line body but expressed with `\n` in a regular
    // string literal — the content reaching the eval driver is
    // identical.
    run(
      """|def g(x: Int): String = eval[String]("\nval doubled = x * 2\nval s = doubled.toString\ns + \"!\"\n")
         |g(7)""".stripMargin
    )
    assertContains("val res0: String = \"14!\"", storedOutput())
  }

  @Test def multiLineBodyIfElseIndented = initially {
    // Indentation-sensitive `if then ... else ...` inside the body.
    // The body's relative indentation is internally consistent so
    // the parser handles it correctly when spliced inside the
    // wrapper's outer braces.
    val body =
      "\n  if n > 0 then" +
      "\n    val a = n * 2" +
      "\n    a + 1" +
      "\n  else" +
      "\n    val b = -n" +
      "\n    b * 3\n"
    run(
      "def h(n: Int): Int = eval[Int](\"\"\"" + body + "\"\"\")\n" +
      "h(5)\n" +
      "h(-2)"
    )
    val out = storedOutput()
    assertContains("val res0: Int = 11", out)
    assertContains("val res1: Int = 6", out)
  }

  @Test def multiLineBodyInClassMethod = initially {
    // Multi-line body inside a class method exercises the
    // `rewriteThisInBody` parse / pretty-print round-trip in
    // addition to the wrapper splice. Two reads + one write to
    // `this.x` interleaved with intermediate vals.
    val body =
      "\n  val before = this.x" +
      "\n  val delta = step * 2" +
      "\n  this.x = this.x + delta" +
      "\n  before\n"
    run(
      "class Counter(var x: Int):\n" +
      "  def advance(step: Int): Int = eval[Int](\"\"\"" + body + "\"\"\")\n" +
      "val c = new Counter(10)\n" +
      "c.advance(3)\n" +
      "c.x"
    )
    val out = storedOutput()
    assertContains("val res0: Int = 10", out)
    assertContains("val res1: Int = 16", out)
  }

  // ---------------------------------------------------------------------------
  // Nested classes (eval inside class B nested in class A).
  //
  // The rewriter walks each enclosing Template and captures BOTH classes'
  // members and type parameters. Each class also pushes its own
  // `__this__<ClassName>` binding so the body's `<ClassName>.this.x` syntax
  // resolves to the right enclosing instance. Path-dependent type renderings
  // (`A.this.B`) are post-processed to the projection form (`A#B`) since the
  // wrapper module isn't lexically inside any of the outer classes.
  // ---------------------------------------------------------------------------

  @Test def nestedClassReadsOuterMember = initially {
    // Eval inside class B (nested in A) should be able to access A's
    // members via the qualified `A.this.<member>` syntax. The
    // rewriter captures both A's and B's members; the outer A's
    // members are bound via `A.this.<name>` so the bind site (which
    // is inside B) reads the right enclosing instance. The body
    // rewrite step translates `A.this.x` → `__this__A.x` (where
    // `__this__A` is the captured `A.this`) and `this.y` →
    // `y__field` for B's own members.
    run(
      """|class A:
         |  val a: Int = 10
         |  class B:
         |    val b: Int = 20
         |    def sum: Int = eval[Int]("A.this.a + this.b")
         |val outer = new A
         |val ab = new outer.B
         |ab.sum""".stripMargin
    )
    assertContains("val res0: Int = 30", storedOutput())
  }

  @Test def nestedClassWritesOuterVar = initially {
    // Mutating an outer-class var from inside a nested class. The
    // outer's `n` is captured under `n__field` (cell-backed); the
    // body's `A.this.n = ...` is rewritten to use that cell.
    run(
      """|class A:
         |  var n: Int = 0
         |  class B:
         |    def bump(): Int = eval[Int]("A.this.n = A.this.n + 1; A.this.n")
         |val outer = new A
         |val ab = new outer.B
         |ab.bump()
         |ab.bump()
         |outer.n""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 1", out)
    assertContains("val res1: Int = 2", out)
    assertContains("val res2: Int = 2", out)
  }

  @Test def classScopeMethodParamShadowsField = initially {
    // Method parameter `x` shadows the class field `x` for the bare
    // name. The body uses `this.x` to access the field; the rewrite
    // routes through `x__field`. The body's `x` (no `this.`) still
    // refers to the method parameter.
    run(
      """|class Box(var x: Int):
         |  def addAndOld(x: Int): Int = eval[Int]("val old = this.x; this.x = this.x + x; old")
         |val b = new Box(10)
         |b.addAndOld(5)
         |b.x""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 10", out)
    assertContains("val res1: Int = 15", out)
  }

  // ---------------------------------------------------------------------------
  // Session-level type aliases (defined at the REPL prompt, NOT in a method).
  // ---------------------------------------------------------------------------

  @Test def sessionTypeAliasPreserved = initially {
    // Session-level aliases (defined at the REPL prompt, not inside
    // a method) are *not* dealiased — they're nameable in the wrapper
    // through the runtime's `import rs$line$N.{given, *}` bridge.
    run("""|type Greeting = String
           |def f(): String =
           |  val s: Greeting = "hi"
           |  eval[String]("s.toUpperCase")
           |f()""".stripMargin)
    assertContains("val res0: String = \"HI\"", storedOutput())
  }

  // ===========================================================================
  // 8. Body-internal definitions: declarations within the eval body.
  // ===========================================================================

  @Test def bodyDefinesLocalVal = initially {
    run("""val r: Int = eval("val a = 3; val b = 4; a * b")""")
    assertContains("val r: Int = 12", storedOutput())
  }

  @Test def bodyDefinesLocalDef = initially {
    run("""val r: Int = eval("def fact(n: Int): Int = if n <= 1 then 1 else n * fact(n - 1); fact(5)")""")
    assertContains("val r: Int = 120", storedOutput())
  }

  @Test def bodyDefinesAndUsesGiven = initially {
    run("""val r: String = eval("given Ordering[Int] = Ordering.Int.reverse; List(3,1,4,1,5).sorted.mkString(\",\")")""")
    assertContains("""val r: String = "5,4,3,1,1"""", storedOutput())
  }

  @Test def bodyDefinesAndUsesExtension = initially {
    run("""val r: Int = eval("extension (x: Int) def squared: Int = x * x; 7.squared")""")
    assertContains("val r: Int = 49", storedOutput())
  }

  @Test def bodyDefinesLazyVal = initially {
    run("""val r: Int = eval("lazy val v = 7; v + v")""")
    assertContains("val r: Int = 14", storedOutput())
  }

  // ===========================================================================
  // 9. Body language features: control flow, pattern matching.
  // ===========================================================================

  @Test def bodyIfElse = initially {
    run("""val r: String = eval("if 5 > 3 then \"yes\" else \"no\"")""")
    assertContains("""val r: String = "yes"""", storedOutput())
  }

  @Test def bodyTryCatch = initially {
    run("""val r: String = eval("try { sys.error(\"boom\") } catch { case e: RuntimeException => e.getMessage }")""")
    assertContains("""val r: String = "boom"""", storedOutput())
  }

  @Test def bodyWhileLoop = initially {
    run("""val r: Int = eval("var i = 0; var s = 0; while i < 5 do { s = s + i; i = i + 1 }; s")""")
    assertContains("val r: Int = 10", storedOutput())
  }

  @Test def bodyPatternMatch = initially {
    run("""val r: Int = eval("List(1,2,3) match { case h :: _ => h ; case Nil => 0 }")""")
    assertContains("val r: Int = 1", storedOutput())
  }

  @Test def bodyMatchWithGuard = initially {
    run("""val r: String = eval("5 match { case n if n > 0 => \"pos\" ; case 0 => \"zero\" ; case _ => \"neg\" }")""")
    assertContains("""val r: String = "pos"""", storedOutput())
  }

  @Test def bodyMatchExtractorTuple = initially {
    // Tuple destructuring in a match case. The extractor `(a, b)`
    // binds `a` and `b` for the case body — these stay inside the
    // body's scope, no rewriter capture needed.
    run("""val r: Int = eval[Int]("(3, 4) match { case (a, b) => a * 10 + b }")""")
    assertContains("val r: Int = 34", storedOutput())
  }

  @Test def bodyMatchListExtractor = initially {
    // Cons / Nil patterns. Recursive shapes: head + tail.
    run("""val r: Int = eval[Int]("List(10, 20, 30) match { case h :: t :: _ => h + t; case _ => 0 }")""")
    assertContains("val r: Int = 30", storedOutput())
  }

  @Test def bodyMatchTypePattern = initially {
    // `case x: SomeType =>` types-narrowing pattern.
    run(
      """val r: String = eval[String]("(42: Any) match { case s: String => \"str:\" + s; case n: Int => \"int:\" + n.toString; case _ => \"other\" }")"""
    )
    assertContains("""val r: String = "int:42"""", storedOutput())
  }

  @Test def bodyMatchOption = initially {
    // Pattern matching on Option with both None and Some(v). The
    // matched option is captured as a lambda parameter (`o`); the
    // match statement INSIDE the eval body operates on the captured
    // value at runtime.
    run(
      """val r: List[String] = List(Some(1), None, Some(2)).map(o => eval[String]("o match { case Some(n) => n.toString; case None => \"-\" }"))"""
    )
    assertContains("""val r: List[String] = List("1", "-", "2")""", storedOutput())
  }

  @Test def bodyMatchSealedHierarchy = initially {
    // Pattern matching on a session-defined sealed hierarchy. The
    // `import rs$line$N.*` runtime bridge brings the hierarchy into
    // the eval body's scope.
    run(
      """|sealed trait Shape
         |case class Circle(r: Double) extends Shape
         |case class Square(s: Double) extends Shape
         |def area(sh: Shape): Double = eval[Double](
         |  "sh match { case Circle(r) => 3.14 * r * r; case Square(s) => s * s }")
         |area(Circle(1.0))
         |area(Square(2.0))""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Double = 3.14", out)
    assertContains("val res1: Double = 4.0", out)
  }

  @Test def bodyMatchNestedExtractor = initially {
    // Nested extractors: case Some((a, b)) => ...
    run(
      """val r: Int = eval[Int]("(Some((3, 4)): Option[(Int, Int)]) match { case Some((a, b)) => a + b; case None => 0 }")"""
    )
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def bodyMatchAtPattern = initially {
    // `case x @ pattern => ...` — the binder `x` references the
    // whole matched object while the inner pattern still narrows.
    run(
      """val r: String = eval[String]("List(1, 2, 3) match { case xs @ (h :: _) => xs.length.toString + \":\" + h.toString; case _ => \"empty\" }")"""
    )
    assertContains("""val r: String = "3:1"""", storedOutput())
  }

  @Test def bodyMatchPipedAlternative = initially {
    // `case A | B =>` alternative patterns (no bindings on either
    // side; binding-with-alternative isn't permitted in Scala).
    run(
      """val r: String = eval[String]("3 match { case 1 | 2 | 3 => \"low\"; case 4 | 5 => \"mid\"; case _ => \"high\" }")"""
    )
    assertContains("""val r: String = "low"""", storedOutput())
  }

  @Test def bodyMatchOnCapturedValue = initially {
    // The body's `match` selector is a captured method parameter.
    // The cases pattern-match on its actual runtime value.
    run(
      """|def classify(input: Any): String = eval[String](
         |  "input match { case _: Int => \"int\"; case _: String => \"str\"; case _: List[?] => \"list\"; case _ => \"other\" }")
         |classify(42)
         |classify("hi")
         |classify(List(1, 2))
         |classify(true)""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val res0: String = "int"""", out)
    assertContains("""val res1: String = "str"""", out)
    assertContains("""val res2: String = "list"""", out)
    assertContains("""val res3: String = "other"""", out)
  }

  @Test def bodyMatchMultipleGuards = initially {
    // Multiple cases each with their own guard.
    run(
      """val r: String = eval[String]("val n = 7; n match { case x if x < 0 => \"neg\"; case 0 => \"zero\"; case x if x % 2 == 0 => \"even\"; case _ => \"odd\" }")"""
    )
    assertContains("""val r: String = "odd"""", storedOutput())
  }

  @Test def bodyForComprehensionYield = initially {
    // For-comprehension with `yield` produces a List. Generators and
    // intermediate bindings work normally inside the body.
    run("""val r: List[Int] = eval("for x <- List(1, 2, 3); y = x * 10 yield x + y")""")
    assertContains("val r: List[Int] = List(11, 22, 33)", storedOutput())
  }

  @Test def bodyForComprehensionWithGuard = initially {
    // Multiple generators + guard. The body parses fine inside the
    // wrapper's braces and behaves identically to a top-level for.
    run("""val r: List[(Int, Int)] = eval("for a <- List(1,2,3); b <- List(10,20) if a + b > 12 yield (a, b)")""")
    assertContains("val r: List[(Int, Int)] = List((1, 20), (2, 20), (3, 10), (3, 20))", storedOutput())
  }

  @Test def bodyForLoopSideEffects = initially {
    // `for` without `yield` runs for side effects. Mutating an outer
    // var (captured) propagates back via the var-cell sync-back.
    run(
      """|var sum: Int = 0
         |eval[Unit]("for x <- 1 to 5 do sum = sum + x")
         |sum""".stripMargin
    )
    // The trailing `sum` shows up via the var-display path
    // (`var sum: Int = 15`) since `sum` is a top-level var.
    assertContains("var sum: Int = 15", storedOutput())
  }

  @Test def nestedEvalAfterTypeAnnotatedVal = initially {
    // Regression: when the outer eval body contained any
    // `AppliedTypeTree` (`List[T]`, `Array[T]`, type-annotated val,
    // etc.), the runtime nested-eval rewrite path went through
    // dotty's pretty-printer, which references `defn.orType` while
    // rendering applied-type trees. Without a fully-initialised
    // `ContextBase` (and a real classpath), that lookup NPEs and the
    // catch-NonFatal in `rewriteUserCode` silently dropped the
    // rewrite, so the inner eval received `enclosingSource = ""` and
    // no bindings. The visible symptom in agent traces was an inner
    // agent referencing an outer-body local that then failed the
    // wrapper compile because the binding wasn't injected.
    val body =
      "val xs: List[Int] = List(1, 2, 3); " +
      "val n: Int = xs.sum; " +
      "eval[Int](\\\"n + 100\\\")"
    run(s"""val r: Int = eval[Int]("$body")""")
    val out = storedOutput()
    assertTrue(s"no compile failure expected, got:\n$out",
      !out.contains("eval failed to compile"))
    assertContains("val r: Int = 106", out)
  }

  @Test def nestedEvalInsideNestedBlocksAndTryCatch = initially {
    // Regression for the agent-trace symptom "the start of this line
    // does not match any of the previous indentation widths". The
    // outer eval body had nested if/else/blocks/try-catch and a def
    // body; the dotty pretty-printer emitted brace-balanced output
    // whose leading-whitespace widths fell *between* the parser's
    // indentation stack entries (e.g. a 17-space line where only 16
    // and 18 were established). Under the indent-significant default
    // the round-trip parse rejected the output and `rewriteUserCode`
    // silently fell back. Forcing `-no-indent` for the round-trip
    // parse makes braces authoritative and the rewrite survives.
    val body =
      "val baseDir = new java.io.File(\\\".\\\"); " +
      "def listOne(d: java.io.File): List[java.io.File] = { " +
      "  val es = d.listFiles; " +
      "  if (es == null) Nil " +
      "  else es.toList.filter(_.getName.endsWith(\\\".txt\\\")) " +
      "}; " +
      "val files = listOne(baseDir); " +
      "val n = try { files.length } catch { case _: Exception => -1 }; " +
      "eval[Int](\\\"n + 1000\\\")"
    run(s"""val r: Int = eval[Int]("$body")""")
    val out = storedOutput()
    assertTrue(s"no compile failure expected, got:\n$out",
      !out.contains("eval failed to compile"))
    assertTrue(s"no rewrite-fallback warning expected, got:\n$out",
      !out.contains("[eval-rewrite] WARNING"))
  }

  @Test def nestedEvalInsideTryFinallyNoCatch = initially {
    // Regression: an outer eval body containing `try { ... eval(...) }
    // finally { ... }` (no catch clause) used to round-trip through
    // the pretty-printer as `try { ... } catch {<empty>} finally
    // { ... }`, which fails to re-parse and silently rolls the rewrite
    // back. The agent-trace symptom is that the inner eval's
    // enclosingSource and bindings are dropped: captured outer-body
    // locals (`x` here) become unresolved at the inner wrapper compile.
    val body =
      "val x = 7; " +
      "val src = scala.io.Source.fromString(\\\"dummy\\\"); " +
      "try eval[Int](\\\"x + 100\\\") finally src.close()"
    run(s"""val r: Int = eval[Int]("$body")""")
    val out = storedOutput()
    assertTrue(s"no compile failure expected, got:\n$out",
      !out.contains("eval failed to compile"))
    assertContains("val r: Int = 107", out)
  }

  @Test def bodyTryCatchFinally = initially {
    // try/catch with multiple cases plus a finally that mutates an
    // outer var. Tests both exception-handling path selection AND
    // var-cell sync-back from a finally block.
    val body =
      "try { throw new IllegalArgumentException(\\\"bad\\\") } " +
      "catch { case _: NullPointerException => \\\"npe\\\"; " +
      "case e: IllegalArgumentException => e.getMessage } " +
      "finally { ran = ran + 1 }"
    run(
      "var ran: Int = 0\n" +
      s"val r: String = eval[String](\"$body\")\n" +
      "r\n" +
      "ran"
    )
    val out = storedOutput()
    assertContains("val r: String = \"bad\"", out)
    // The trailing `ran` references the top-level var; the REPL
    // shows it via the var-display path.
    assertContains("var ran: Int = 1", out)
  }

  @Test def bodyIfElseIfChain = initially {
    // if / else if / else chain.
    run("""val r: String = eval[String]("val n = 7; if n < 0 then \"neg\" else if n == 0 then \"zero\" else if n < 10 then \"small\" else \"big\"")""")
    assertContains("""val r: String = "small"""", storedOutput())
  }

  @Test def bodyNestedWhile = initially {
    // Nested while loops accumulating into an outer-captured var.
    run(
      """|var total: Int = 0
         |eval[Unit]("var i = 0; while i < 3 do { var j = 0; while j < 3 do { total = total + 1; j = j + 1 }; i = i + 1 }")
         |total""".stripMargin
    )
    assertContains("var total: Int = 9", storedOutput())
  }

  @Test def bodyDoWhile = initially {
    // Scala 3 doesn't have `do { ... } while (...)` syntax, but the
    // run-once-then-check idiom still works via a regular while.
    run("""val r: Int = eval[Int]("var n = 5; var p = 1; while { p = p * n; n = n - 1; n > 0 } do (); p")""")
    assertContains("val r: Int = 120", storedOutput())
  }

  // ===========================================================================
  // 9b. Recursion, both INSIDE and OUTSIDE eval.
  // ===========================================================================

  @Test def bodyRecursiveDef = initially {
    // The body declares its own recursive `def`. (`bodyDefinesLocalDef`
    // already covers a one-shot recursive def with factorial; here we
    // exercise tail-recursion-shaped code.)
    run("""val r: Int = eval[Int]("def loop(n: Int, acc: Int): Int = if n == 0 then acc else loop(n - 1, acc + n); loop(10, 0)")""")
    assertContains("val r: Int = 55", storedOutput())
  }

  @Test def bodyMutuallyRecursiveDefs = initially {
    // Mutually recursive `def`s declared inside the body. Compiled in
    // the wrapper as locals; calls resolve forward and backward.
    run(
      """val r: Boolean = eval[Boolean]("def isEven(n: Int): Boolean = if n == 0 then true else isOdd(n - 1); def isOdd(n: Int): Boolean = if n == 0 then false else isEven(n - 1); isEven(10)")"""
    )
    assertContains("val r: Boolean = true", storedOutput())
  }

  @Test def bodyCallsOuterRecursiveMethod = initially {
    // The body calls a REPL-session-level recursive method. Classes
    // and recursive methods on the session level are reached via the
    // `import rs$line$N.*` runtime bridge; the eval body just sees
    // the name `fib` in scope.
    run(
      """|def fib(n: Int): Int = if n < 2 then n else fib(n - 1) + fib(n - 2)
         |val r: Int = eval[Int]("fib(10)")""".stripMargin
    )
    assertContains("val r: Int = 55", storedOutput())
  }

  @Test def bodyCallsOuterRecursiveMethodWithCapturedParam = initially {
    // The body's call passes a *captured* lambda parameter as the
    // argument to the recursive method.
    run(
      """|def fact(n: Int): Int = if n <= 1 then 1 else n * fact(n - 1)
         |val r: List[Int] = List(0, 1, 4, 5).map(z => eval[Int]("fact(z)"))""".stripMargin
    )
    assertContains("val r: List[Int] = List(1, 1, 24, 120)", storedOutput())
  }

  @Test def evalInsideRecursiveMethod = initially {
    // The recursive method itself contains an eval call. Each
    // recursion step issues a fresh eval; the captured `n` is
    // re-bound per call.
    run(
      """|def countDown(n: Int): String =
         |  if n == 0 then "done"
         |  else eval[String]("countDown(n - 1)")
         |countDown(3)""".stripMargin
    )
    assertContains("""val res0: String = "done"""", storedOutput())
  }

  @Test def evalInsideTailRecursiveMethod = initially {
    // Captures the lambda parameter and recurses through eval. The
    // captured `acc` is the running accumulator.
    run(
      """|def sumTo(n: Int, acc: Int): Int =
         |  if n == 0 then acc
         |  else eval[Int]("sumTo(n - 1, acc + n)")
         |sumTo(10, 0)""".stripMargin
    )
    assertContains("val res0: Int = 55", storedOutput())
  }

  // ===========================================================================
  // 9c. Control flow OUTSIDE the eval call (eval inside while/for/try/if).
  //
  // The rewriter walks lambda / block / DefDef scopes regardless of the
  // surrounding control structure, so an eval inside a `while` body or a
  // `for` comprehension's expression captures the right names.
  // ===========================================================================

  @Test def whileLoopAroundEvalMutatesCapturedVar = initially {
    // The eval call lives inside a while-loop body. Each iteration
    // re-binds the (cell-backed) outer var and propagates back.
    run(
      """|def f(): Int =
         |  var i: Int = 0
         |  var n: Int = 0
         |  while i < 5 do
         |    eval[Unit]("n = n + i")
         |    i = i + 1
         |  n
         |f()""".stripMargin
    )
    assertContains("val res0: Int = 10", storedOutput())
  }

  @Test def forLoopAroundEvalCallsEachIteration = initially {
    // The eval is the expression body of a `for ... yield`. Each
    // iteration captures the for-binding `x` lambda-style and
    // produces one element.
    run("""val r: List[Int] = (for x <- List(1, 2, 3, 4) yield eval[Int]("x * x"))""")
    assertContains("val r: List[Int] = List(1, 4, 9, 16)", storedOutput())
  }

  @Test def forLoopWithMultipleGeneratorsAroundEval = initially {
    // Multiple generators: both `a` and `b` are in scope as captured
    // bindings inside the eval body.
    run(
      """val r: List[Int] =
        |  (for a <- List(1, 2); b <- List(10, 20) yield eval[Int]("a * 100 + b"))""".stripMargin
    )
    assertContains("val r: List[Int] = List(110, 120, 210, 220)", storedOutput())
  }

  @Test def tryAroundEvalCatchingEvalCompileError = initially {
    // try/catch surrounds eval. `EvalCompileException` is a normal
    // RuntimeException; user code can catch it just like any other.
    run(
      """|import dotty.tools.repl.EvalCompileException
         |def safeEval(): String =
         |  try eval[Int]("undefinedSymbol").toString
         |  catch case _: EvalCompileException => "compile-failed"
         |safeEval()""".stripMargin
    )
    assertContains("""val res0: String = "compile-failed"""", storedOutput())
  }

  @Test def ifBranchesEachCallEval = initially {
    // Both branches of an if call eval; the captured method param is
    // visible in either branch's body.
    run(
      """|def classify(n: Int): String =
         |  if n >= 0 then eval[String]("\"non-negative: \" + n.toString")
         |  else eval[String]("\"negative: \" + n.toString")
         |classify(5)
         |classify(-3)""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val res0: String = "non-negative: 5"""", out)
    assertContains("""val res1: String = "negative: -3"""", out)
  }

  @Test def matchExpressionAroundEval = initially {
    // Each case of a `match` calls eval. Binding-by-pattern (e.g.
    // `case Some(v) =>` introducing `v`) is *not* picked up by the
    // rewriter (case-pattern names aren't on the scope stack), so
    // the body has to reach values via the matched variable. Here
    // the body uses the captured method parameter `opt` directly.
    run(
      """|def describe(opt: Option[Int]): String = opt match
         |  case Some(_) => eval[String]("\"got \" + opt.get.toString")
         |  case None    => eval[String]("\"none\"")
         |describe(Some(7))
         |describe(None)""".stripMargin
    )
    val out = storedOutput()
    assertContains("""val res0: String = "got 7"""", out)
    assertContains("""val res1: String = "none"""", out)
  }

  // ===========================================================================
  // 10. Functions defined inside eval, returned and used outside.
  // ===========================================================================

  @Test def returnsAnonymousFunction = initially {
    run("""|val f: Int => Int = eval("(x: Int) => x * 2")
           |val r: Int = f(7)""".stripMargin)
    assertContains("val r: Int = 14", storedOutput())
  }

  @Test def returnsNamedFunction = initially {
    run("""|val f: Int => Int = eval("def myFn(x: Int): Int = x * x; myFn")
           |val r: Int = f(5)""".stripMargin)
    assertContains("val r: Int = 25", storedOutput())
  }

  @Test def returnsCurriedFunction = initially {
    run("""|val add: Int => (Int => Int) = eval("(a: Int) => (b: Int) => a + b")
           |val r: Int = add(3)(4)""".stripMargin)
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def returnedFunctionClosesOverReplState = initially {
    // The lambda captures REPL-defined `factor` at its compile time; calling
    // it later still picks up the captured value.
    run("val factor: Int = 10")
  } andThen {
    storedOutput()
    run("""|val f: Int => Int = eval("(x: Int) => x * factor")
           |val r: Int = f(7)""".stripMargin)
    assertContains("val r: Int = 70", storedOutput())
  }

  @Test def returnsCurriedClosureBuiltDynamically = initially {
    // The motivating example: a method takes a string operator name
    // and returns a curried `Int => Int => Int` built at call time
    // by an `eval` body that interpolates the operator into the
    // function literal. This exercises:
    //   - dynamic body construction via `s"..."`,
    //   - the wrapper returning a closure value,
    //   - the closure crossing the eval / REPL classloader boundary
    //     as `scala.Function1` (visible as the same Class on both
    //     sides because the AbstractFileClassLoader delegates
    //     `scala.*` to the parent loader).
    run(
      """|def mkOp(op: String): Int => Int => Int =
         |  eval[Int => Int => Int](s"i => j => i $op j")
         |val plus = mkOp("+")
         |val times = mkOp("*")
         |plus(2)(3)
         |times(4)(5)""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 5", out)
    assertContains("val res1: Int = 20", out)
  }

  @Test def returnsClosureCapturingMethodParam = initially {
    // The closure built inside eval captures the method parameter
    // `n`. Calling the returned closure later applies it to `n`,
    // even though `n` is no longer in scope at the call site.
    run(
      """|def adderFor(n: Int): Int => Int =
         |  eval[Int => Int]("(x: Int) => x + n")
         |val addFive = adderFor(5)
         |val addTen = adderFor(10)
         |addFive(100)
         |addTen(100)""".stripMargin
    )
    val out = storedOutput()
    assertContains("val res0: Int = 105", out)
    assertContains("val res1: Int = 110", out)
  }

  @Test def returnsClosureMultiArgPartiallyApplied = initially {
    // A closure of higher arity returned from eval, then partially
    // applied via a curried-style helper.
    run(
      """|val f3: (Int, Int, Int) => Int =
         |  eval[(Int, Int, Int) => Int]("(a: Int, b: Int, c: Int) => a * 100 + b * 10 + c")
         |f3(1, 2, 3)""".stripMargin
    )
    assertContains("val res0: Int = 123", storedOutput())
  }

  @Test def returnsListOfClosures = initially {
    // The body returns a list of closures, each capturing a
    // different value.
    run(
      """|val fs: List[Int => Int] = eval[List[Int => Int]]("List(1, 2, 3).map(k => (x: Int) => x + k)")
         |fs.map(f => f(100))""".stripMargin
    )
    assertContains("val res0: List[Int] = List(101, 102, 103)", storedOutput())
  }

  @Test def returnedClosureCalledRepeatedly = initially {
    // The same closure is invoked many times after being returned;
    // each call re-runs the closure body (no side effects from the
    // construction beyond what eval itself did once).
    run(
      """|val incr: Int => Int = eval[Int => Int]("(x: Int) => x + 1")
         |val r: List[Int] = List(0, 1, 2, 3).map(incr)""".stripMargin
    )
    assertContains("val r: List[Int] = List(1, 2, 3, 4)", storedOutput())
  }

  // ===========================================================================
  // 11. Generic and stdlib types inside the body.
  // ===========================================================================

  @Test def stdlibListSum = initially {
    run("""val r: Int = eval("List(1, 2, 3, 4).map(_ * 2).sum")""")
    assertContains("val r: Int = 20", storedOutput())
  }

  @Test def stdlibForComprehension = initially {
    run("""val r: List[Int] = eval("(for i <- 1 to 3; j <- 1 to i yield i * j).toList")""")
    assertContains("List(1, 2, 4, 3, 6, 9)", storedOutput())
  }

  @Test def stdlibOptionMap = initially {
    run("""val r: Int = eval("Some(5).map(_ * 3).getOrElse(0)")""")
    assertContains("val r: Int = 15", storedOutput())
  }

  // ===========================================================================
  // 12. Compile-time errors in the eval body.
  // ===========================================================================

  @Test def compileErrorUnknownIdent = initially {
    run("""val r: Int = eval("nonexistentVar + 1")""")
    assertContains("nonexistentVar", storedOutput())
  }

  @Test def compileErrorParseFailure = initially {
    run("""val r: Int = eval("(1 + ")""")
    val out = storedOutput()
    assertTrue(s"expected an error, got:\n$out", out.contains("failed to compile") || out.contains("Error"))
  }

  @Test def compileErrorTypeMismatch = initially {
    run("""val r: Int = eval("\"a string\" + 1: Int")""")
    val out = storedOutput()
    assertTrue(s"expected an error, got:\n$out",
      out.contains("failed to compile") || out.contains("Error") || out.contains("ClassCastException"))
  }

  @Test def compileErrorIsEvalCompileException = initially {
    // Compile-time errors arrive as `EvalCompileException`, not the
    // generic `RuntimeException`. The exception type is shared across
    // the eval / REPL classloader boundary so the user can catch it
    // by name and inspect its structured `errors` field.
    run(
      """|import dotty.tools.repl.EvalCompileException
         |val r: String =
         |  try
         |    eval[Int]("false")
         |    "no error"
         |  catch case e: EvalCompileException =>
         |    s"caught ${e.errors.length} error(s)"""".stripMargin
    )
    assertContains("""val r: String = "caught 1 error(s)"""", storedOutput())
  }

  // ===========================================================================
  // 13a. Nested eval: an eval body can itself call eval.
  // ===========================================================================

  @Test def nestedEvalSimple = initially {
    run("""val r: Int = eval("eval[Int](\"1 + 2\") * 10")""")
    assertContains("val r: Int = 30", storedOutput())
  }

  @Test def nestedEvalThreeDeep = initially {
    run("""val r: Int = eval("eval[Int](\"eval[Int](\\\"3\\\") + 4\") * 2")""")
    assertContains("val r: Int = 14", storedOutput())
  }

  @Test def nestedEvalSeesReplState = initially {
    run("""|val n: Int = 7
           |val r: Int = eval("eval[Int](\"n * n\")")""".stripMargin)
    assertContains("val r: Int = 49", storedOutput())
  }

  @Test def nestedEvalSeesOuterBodyVal = initially {
    // The outer eval body declares `val j`; the inner eval inside that
    // body must capture it. The eval driver runs the rewriter over the
    // body, so the inner call has `j` injected as a binding.
    run("""|val i: Int = 1
           |val r: Int = eval[Int]("val j = 2; eval[Int](\"i + j\")")""".stripMargin)
    assertContains("val r: Int = 3", storedOutput())
  }

  @Test def nestedEvalSeesOuterBodyVar = initially {
    // Same idea for a `var` declared in the outer body: captured as a
    // cell into the inner eval, mutation propagates.
    run("""val r: Int = eval[Int]("var k = 0; eval[Unit](\"k = k + 7\"); k")""")
    assertContains("val r: Int = 7", storedOutput())
  }

  @Test def nestedEvalDynamicInner = initially {
    run("""|val inner: String = "1 + 2"
           |val outer: String = "eval[Int](inner)"
           |val r: Int = eval(outer)""".stripMargin)
    assertContains("val r: Int = 3", storedOutput())
  }

  // ===========================================================================
  // 13b. Larger showcase: a symbolic-differentiation engine driven
  //      entirely by recursive `eval` calls.
  //
  // Why this is interesting:
  //   * The user's code defines a small AST (`Expr` algebra: X, Num,
  //     Add, Mul, Pow) plus three pieces of plumbing:
  //       - `show(e)`  prints an `Expr` as a Scala expression string,
  //                    using `x` as the variable name;
  //       - `lit(e)`   prints an `Expr` as the SCALA SOURCE that
  //                    reconstructs it (`Add(Mul(Num(3), Pow(X, 2)), ...)`);
  //       - `diff(e)`  symbolic differentiation on the AST.
  //
  //   * `evalAt(e, x)` evaluates the expression at a runtime point by
  //     building Scala source `val x = <x>; <show(e)>` and handing it
  //     to `eval[Int]`. Both the variable's value AND the body's text
  //     come from runtime data — so this isn't something a macro or
  //     compile-time inline could do.
  //
  //   * `diffAt(e, n, x)` computes the nth derivative at `x`. For
  //     `n > 0` it RECURSES THROUGH EVAL: the eval body interpolates
  //     `diff(<e-as-source>)`, the new `n - 1`, and the same `x`,
  //     then calls `diffAt` again. Each level of recursion spawns a
  //     fresh wrapper compile that itself invokes `eval`, so an Nth
  //     derivative produces N+1 nested-eval invocations.
  //
  //   * Type safety is preserved end-to-end: `eval[Int]` ascribes the
  //     return type, the eval driver type-checks the spliced body
  //     against `Int`, and a body that returned the wrong type would
  //     fail at the eval-driver compile, not silently as a
  //     `ClassCastException` at the call site.
  //
  // The polynomial used: f(x) = 3x² + 5
  //   f(2)  = 17       — `evalAt(f, 2)`
  //   f'(2) = 6·2 = 12 — `evalAt(diff(f), 2)`
  //   f''(7) = 6       — `diffAt(f, 2, 7)` (recurses twice through eval)
  // ===========================================================================

  @Test def symbolicDifferentiationViaRecursiveEval = initially {
    val program =
      """|sealed trait Expr
         |case object X extends Expr
         |case class Num(v: Int) extends Expr
         |case class Add(a: Expr, b: Expr) extends Expr
         |case class Mul(a: Expr, b: Expr) extends Expr
         |case class Pow(b: Expr, n: Int) extends Expr
         |
         |def show(e: Expr): String = e match
         |  case X => "x"
         |  case Num(v) => v.toString
         |  case Add(a, b) => s"(${show(a)} + ${show(b)})"
         |  case Mul(a, b) => s"(${show(a)} * ${show(b)})"
         |  case Pow(b, n) =>
         |    if n == 0 then "1"
         |    else if n == 1 then show(b)
         |    else (1 until n).foldLeft(show(b))((acc, _) => s"($acc * ${show(b)})")
         |
         |def lit(e: Expr): String = e match
         |  case X => "X"
         |  case Num(v) => s"Num($v)"
         |  case Add(a, b) => s"Add(${lit(a)}, ${lit(b)})"
         |  case Mul(a, b) => s"Mul(${lit(a)}, ${lit(b)})"
         |  case Pow(b, n) => s"Pow(${lit(b)}, $n)"
         |
         |def diff(e: Expr): Expr = e match
         |  case X => Num(1)
         |  case Num(_) => Num(0)
         |  case Add(a, b) => Add(diff(a), diff(b))
         |  case Mul(a, b) => Add(Mul(diff(a), b), Mul(a, diff(b)))
         |  case Pow(b, n) => Mul(Mul(Num(n), Pow(b, n - 1)), diff(b))
         |
         |def evalAt(e: Expr, x: Int): Int =
         |  eval[Int](s"val x = $x; ${show(e)}")
         |
         |def diffAt(e: Expr, n: Int, x: Int): Int =
         |  if n == 0 then evalAt(e, x)
         |  else eval[Int](s"diffAt(diff(${lit(e)}), ${n - 1}, $x)")
         |
         |val f: Expr = Add(Mul(Num(3), Pow(X, 2)), Num(5))
         |evalAt(f, 2)
         |evalAt(diff(f), 2)
         |diffAt(f, 0, 2)
         |diffAt(f, 1, 2)
         |diffAt(f, 2, 7)
         |""".stripMargin
    run(program)
    val out = storedOutput()
    // f(2)  = 3*4 + 5
    assertContains("val res0: Int = 17", out)
    // f'(2) = 6x at x=2
    assertContains("val res1: Int = 12", out)
    // diffAt(f, 0, 2)  = f(2) — a fresh eval roundtrip, no recursion
    assertContains("val res2: Int = 17", out)
    // diffAt(f, 1, 2)  = f'(2) — one level of recursive eval
    assertContains("val res3: Int = 12", out)
    // diffAt(f, 2, 7)  = f''(7) = 6 — two levels of recursive eval
    assertContains("val res4: Int = 6", out)
  }

  // ===========================================================================
  // 13. Runtime errors raised by the eval body propagate to the caller.
  // ===========================================================================

  @Test def runtimeArithmeticException = initially {
    run("""val r: Int = eval("1 / 0")""")
    assertContains("ArithmeticException", storedOutput())
  }

  @Test def runtimeUserThrow = initially {
    run("""val r: Int = eval("throw new RuntimeException(\"user-boom\"); 0")""")
    assertContains("user-boom", storedOutput())
  }

  // ===========================================================================
  // 14. Failed previous lines don't break later eval calls.
  //
  // When a REPL line fails to type-check, its `rs$line$N` wrapper has no
  // classfile. The runtime filters such indexes out before generating the
  // synthetic `import rs$line$N.*` so a later `eval(...)` doesn't trip on
  // the missing wrapper.
  // ===========================================================================

  @Test def evalAfterFailedReplLine = initially {
    // First line fails type-check.
    run("val broken = nonexistent + 1")
  } andThen {
    storedOutput()
    // The next eval still works: no spurious "Not found: rs$line$1".
    run("""val r: Int = eval("1 + 2")""")
    assertContains("val r: Int = 3", storedOutput())
  }

end DynamicEvalTests

/** `eval`'s isolated compile inherits the live REPL's CLI flags, so
 *  options like `-Yexplicit-nulls` apply inside the body too: `null`
 *  doesn't conform to `String` once explicit-nulls is on, so a body that
 *  binds `val s: String = null` fails to compile.
 */
class DynamicEvalExplicitNullsTests extends ReplTest(
  ReplTest.defaultOptions ++ Array("-Yexplicit-nulls")
):
  @Test def explicitNullsForwardedToEval =
    initially {
      run("""val r: String = eval("val s: String = null; s")""")
      val out = storedOutput()
      assertTrue(
        s"expected an explicit-nulls error, got:\n$out",
        out.contains("Null") || out.contains("Found:") || out.contains("failed to compile")
      )
    }
end DynamicEvalExplicitNullsTests

/** Capture-checking is enabled at the REPL via
 *  `-language:experimental.captureChecking`. The flag is forwarded into the
 *  eval driver's compilation, so a capture-violating body fails with a
 *  capture-checker diagnostic rather than silently compiling.
 */
class DynamicEvalCaptureCheckingTests extends ReplTest(
  ReplTest.defaultOptions ++ Array("-language:experimental.captureChecking")
):
  @Test def pureFunctionTypeAvailableInEval =
    // `->` is the pure-function arrow that only parses under
    // `experimental.captureChecking`; if the flag wasn't forwarded the
    // body would fail with a syntax / undefined-type error instead of
    // compiling.
    initially {
      run("""val r: Int = eval("val f: Int -> Int = (n: Int) => n + 1; f(41)")""")
      assertContains("val r: Int = 42", storedOutput())
    }

  @Test def captureViolationDetectedInEval =
    initially {
      run(
        """|import caps.*
           |class IO extends SharedCapability
           |val io: IO = new IO""".stripMargin
      )
    } andThen {
      storedOutput()
      // Pure function (`->`) capturing `io` (a SharedCapability) is what
      // capture checking rejects.
      run("""val r = eval("val f: () -> String = () => io.toString; f()")""")
      val out = storedOutput()
      assertTrue(
        s"expected a capture-checking error, got:\n$out",
        out.contains("captures") ||
          out.contains("capability") ||
          out.contains("flow") ||
          out.contains("failed to compile")
      )
    }

  // ===========================================================================
  // Capture-faithful eval via the verification compile pass
  //
  // The runtime splices the (now known) eval body string back into a
  // copy of the enclosing top-level statement and re-typechecks the
  // result under the original lexical context. That catches CC
  // violations the wrapper-compile path can't see, because the wrapper
  // erases capture sets on binding-parameter types (an `IO^` parameter
  // arrives as plain `IO`), so a pure-function position rejecting the
  // capability never gets a chance to fire there.
  // ===========================================================================

  @Test def captureViolationOnIoParamInsidePureLambdaRejected =
    initially {
      run(
        """|import caps.*
           |trait C[T]:
           |  def map[U](op: T -> U): C[U] = ???
           |class IO extends SharedCapability
           |class CImpl extends C[Int]
           |def f(i: Int, io: IO^, c: C[Int]) =
           |  eval[Any]("c.map(x => io.toString)")
           |f(1, new IO, new CImpl)""".stripMargin
      )
      val out = storedOutput()
      // The body `x => io.toString` captures the `io: IO^` parameter
      // inside an `op: T -> U` (pure) position. The wrapper compile
      // erases the `^` on the binding type and so doesn't see it; the
      // verification pass re-checks the original `def f(i: Int, io: IO^, ...)`
      // with the body inlined and rejects.
      assertTrue(
        s"expected a capture-checking failure, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("captures") || out.contains("capability") || out.contains("flow"))
      )
    }

  @Test def pureBodyOnIoParamAccepted =
    initially {
      run(
        """|import caps.*
           |trait C[T]:
           |  def map[U](op: T -> U): C[U] = ???
           |class IO extends SharedCapability
           |class CImpl extends C[Int]:
           |  override def map[U](op: Int -> U): C[U] = new CImpl().asInstanceOf[C[U]]
           |def g(i: Int, io: IO^, c: C[Int]) =
           |  eval[Any]("c.map(x => i)")
           |g(1, new IO, new CImpl)
           |println("g succeeded")""".stripMargin
      )
      val out = storedOutput()
      // `x => i` only captures the pure `Int` parameter `i`. The
      // verification compile and the wrapper compile both accept it.
      assertContains("g succeeded", out)
      assertTrue(
        s"expected no eval failure, got:\n$out",
        !out.contains("failed to compile")
      )
    }

  @Test def captureViolationInNestedEvalOnly =
    initially {
      // The outer eval body is a plain `eval[Any]("...")` call — no
      // CC concerns *at that level*. The CC violation lives inside
      // the inner eval body: `c.map(x => io.toString)` captures `io`
      // into a `T -> U` (pure) lambda position. This exercises the
      // nested-context chaining: the outer eval's `enclosingSource`
      // is `def f(...) = <Marker>`, and when the rewriter walks the
      // outer body it composes
      //   def f(...) = ({ <outerBodyWithInnerMarker> })
      // as the inner eval's `enclosingSource`. The inner verification
      // pass splices the inner body in and CC sees the full original
      // context (def + outer body + inner body) at once.
      run(
        """|import caps.*
           |trait C[T]:
           |  def map[U](op: T -> U): C[U] = ???
           |class IO extends SharedCapability
           |class CImpl extends C[Int]
           |def f(i: Int, io: IO^, c: C[Int]): Any =
           |  eval[Any]("eval[Any](\"c.map(x => io.toString)\")")
           |f(1, new IO, new CImpl)""".stripMargin
      )
      val out = storedOutput()
      assertTrue(
        s"expected a nested capture-checking failure, got:\n$out",
        out.contains("failed to compile") &&
          (out.contains("captures") || out.contains("capability") || out.contains("flow"))
      )
    }
end DynamicEvalCaptureCheckingTests

/** Tests for the agent / LLM workflow APIs:
 *
 *    - Closure form `eval(gen: EvalContext => String)`: lets a
 *      generator inspect the enclosing source, the placeholder
 *      marker, and the captured bindings before producing the body.
 *    - Non-throwing `evalSafe[T]: EvalResult[T]`: lets the caller
 *      branch on `isSuccess` / `isFailure` and feed
 *      `error.errors` back into the generator instead of catching.
 */
class DynamicEvalAgentApiTests extends ReplTest:

  @Test def closureFormSeesEnclosingSourceAndPlaceholder =
    initially {
      run(
        """|import dotty.tools.repl.EvalContext
           |val r: Int = eval { (ctx: EvalContext) =>
           |  // The agent would inspect ctx.enclosingSource (and
           |  // ctx.placeholder for where to splice) to compose its
           |  // prompt. Here we just assert the marker is present.
           |  assert(ctx.enclosingSource.contains(ctx.placeholder),
           |    s"placeholder ${ctx.placeholder} missing from ${ctx.enclosingSource}")
           |  "100 + 23"
           |}
           |println(s"r=$r")""".stripMargin
      )
      assertContains("r=123", storedOutput())
    }

  @Test def closureFormSeesBindingNames =
    initially {
      run(
        """|import dotty.tools.repl.EvalContext
           |def add(x: Int, y: Int): Int =
           |  eval { (ctx: EvalContext) =>
           |    // The generator can see the in-scope names.
           |    assert(ctx.bindings.map(_.name).toSet == Set("x", "y"),
           |      s"got ${ctx.bindings.map(_.name).toList}")
           |    "x + y"
           |  }
           |println(s"add(7, 35)=${add(7, 35)}")""".stripMargin
      )
      assertContains("add(7, 35)=42", storedOutput())
    }

  @Test def closureFormCaptureCheckingStillFires =
    // Body comes from the generator at runtime, but the rewriter still
    // captures the enclosing-source slice — so the verification pass
    // catches CC violations exactly the same way as the literal-string
    // form. This test runs *without* the captureChecking flag, so we
    // just check the body composes correctly.
    initially {
      run(
        """|import dotty.tools.repl.EvalContext
           |def greet(name: String): String =
           |  eval[String] { (ctx: EvalContext) =>
           |    s"\"hello, \" + name"
           |  }
           |println(greet("world"))""".stripMargin
      )
      assertContains("hello, world", storedOutput())
    }

  @Test def evalSafeReturnsValueOnSuccess =
    initially {
      run(
        """|import dotty.tools.repl.EvalResult
           |val r: EvalResult[Int] = evalSafe[Int]("1 + 41")
           |println(s"isSuccess=${r.isSuccess}, get=${r.get}")""".stripMargin
      )
      assertContains("isSuccess=true, get=42", storedOutput())
    }

  @Test def evalSafeReturnsErrorOnCompileFailure =
    initially {
      run(
        """|import dotty.tools.repl.{Eval, EvalResult}
           |val r: EvalResult[Int] = evalSafe[Int]("nonExistentSym + 1")
           |val e: Eval.CompileFailure | Null = r.error
           |println(s"isFailure=${r.isFailure}, errors=${e.nn.errors.length}")
           |println(s"first=${e.nn.errors(0).split('\n').head}")""".stripMargin
      )
      val out = storedOutput()
      assertContains("isFailure=true, errors=1", out)
      assertContains("Not found: nonExistentSym", out)
    }

  @Test def evalSafeAgentRetryLoop =
    // The motivating use case: an agent generates code, the eval
    // fails to compile, the agent inspects the error and generates
    // again. Modeled here with two attempts, the first deliberately
    // bad and the second corrected.
    initially {
      run(
        """|import dotty.tools.repl.{EvalContext, EvalResult}
           |var attempt: Int = 0
           |val r: EvalResult[Int] = evalSafe[Int] { (ctx: EvalContext) =>
           |  attempt += 1
           |  if attempt == 1 then "definitelyNotDefined + 1"
           |  else "21 * 2"
           |}
           |val r2: EvalResult[Int] =
           |  if r.isSuccess then r
           |  else
           |    // "agent" retries, having seen the error.
           |    val errMsg = r.error.nn.errors.mkString("|")
           |    println(s"retrying after: ${errMsg.split('\n').head}")
           |    evalSafe[Int] { (ctx: EvalContext) =>
           |      // For this test the closure ignores the error and
           |      // produces a known-good body.
           |      "21 * 2"
           |    }
           |println(s"final=${r2.get}")""".stripMargin
      )
      val out = storedOutput()
      assertContains("retrying after:", out)
      assertContains("final=42", out)
    }

  @Test def evalSafeClosureFormSeesContext =
    initially {
      run(
        """|import dotty.tools.repl.{EvalContext, EvalResult}
           |def f(x: Int): EvalResult[Int] =
           |  evalSafe[Int] { (ctx: EvalContext) =>
           |    // The generator decides what to splice based on the
           |    // captured bindings.
           |    assert(ctx.bindings.map(_.name).toSet == Set("x"),
           |      s"expected [x], got ${ctx.bindings.map(_.name).toList}")
           |    assert(ctx.enclosingSource.nonEmpty,
           |      "expected non-empty enclosing source for a def-bound eval")
           |    "x * x"
           |  }
           |val r = f(7)
           |println(s"f(7)=${r.get}")""".stripMargin
      )
      assertContains("f(7)=49", storedOutput())
    }

  @Test def evalSafeDoesNotCaptureNestedCompileFailure =
    // The outer call is evalSafe; the body contains a *nested* eval
    // (not evalSafe) that fails to compile. The nested compile
    // failure surfaces as a thrown EvalCompileException at runtime
    // — that's the body's runtime exception, not the outer's compile
    // state. evalSafe must propagate it, not wrap it as
    // `EvalResult.failure` (which would tell the agent "your outer
    // code didn't compile" when in fact the outer did and the body
    // crashed).
    initially {
      run(
        """|import dotty.tools.repl.{EvalResult, EvalCompileException}
           |val outcome: String =
           |  try
           |    val r = evalSafe[Int]("eval[Int](\"undefinedSym + 1\")")
           |    if r.isFailure then "WRONG: outer evalSafe captured nested failure"
           |    else "WRONG: produced a value"
           |  catch case _: EvalCompileException =>
           |    "OK: nested failure propagated through outer evalSafe"
           |println(outcome)""".stripMargin
      )
      assertContains("OK: nested failure propagated", storedOutput())
    }

  @Test def evalSafeCapturesOwnCompileFailure =
    // Sanity check on the other side: a real outer-compile error
    // (here, a body that references an undefined symbol with no
    // nesting involved) IS captured by evalSafe, since it is the
    // outer call's own compile state.
    initially {
      run(
        """|import dotty.tools.repl.EvalResult
           |val r: EvalResult[Int] = evalSafe[Int]("undefinedTopLevel + 1")
           |println(s"isFailure=${r.isFailure}, count=${r.error.nn.errors.length}")""".stripMargin
      )
      assertContains("isFailure=true, count=1", storedOutput())
    }

  @Test def closureFormInNestedEvalSeesChainedContext =
    // Inside an outer eval's body, a nested eval can also use the
    // closure form. The runtime nested-eval rewriter (rewriteCode in
    // Eval.scala) composes the inner enclosingSource so it includes
    // the outer's enclosing-source plus the outer body wrapper. The
    // inner closure should see the full chain (containing the outer
    // def signature and the outer `({ ... })` wrapper) plus all the
    // bindings the outer captured. The test avoids `s"..."`
    // interpolation inside the inner body string because the
    // rewriter's pretty-printer does not always round-trip those
    // (see EVAL.md "Pretty-printer round-trip in nested eval").
    initially {
      val q3 = "\"\"\""
      val innerBody =
        s"""${q3}eval[Int] { (innerCtx: dotty.tools.repl.EvalContext) =>
           |      assert(innerCtx.bindings.map(_.name).toSet == Set("i"))
           |      assert(innerCtx.enclosingSource.contains("def f(i: Int)"),
           |        "inner should also see the def signature (chained from outer)")
           |      assert(innerCtx.enclosingSource.contains("({ "),
           |        "inner enclosing should include the outer-body wrapper braces")
           |      "i + 1"
           |    }${q3}""".stripMargin
      run(
        s"""|import dotty.tools.repl.EvalContext
            |def f(i: Int): Int =
            |  eval[Int] { (outerCtx: EvalContext) =>
            |    assert(outerCtx.bindings.map(_.name).toSet == Set("i"))
            |    assert(outerCtx.enclosingSource.contains("def f(i: Int)"),
            |      "outer should see the def signature")
            |    $innerBody
            |  }
            |println("f(10) = " + f(10))""".stripMargin
      )
      assertContains("f(10) = 11", storedOutput())
    }

  @Test def evalSafeRetryInsideNestedBodyKeepsChainedContext =
    // Models a retry loop *inside* an outer eval body. The outer body
    // declares `val x` and then calls `evalSafe` twice (the second
    // call is the "retry" after the first compile-failed). Both
    // evalSafe calls must see the chained context (def signature +
    // outer body's `val x`); the second call's bindings additionally
    // include `r1`, the val sitting between the two calls in the same
    // block. Verifies that running through the rewriter twice (once
    // for outer parsing, once for nested-body rewriting) doesn't
    // collapse the chain on the retry.
    initially {
      val q3 = "\"\"\""
      val innerBody =
        s"""${q3}val x = 5
           |    val r1 = evalSafe[Int] { (ctx1: dotty.tools.repl.EvalContext) =>
           |      assert(ctx1.bindings.map(_.name).toSet == Set("x", "i"),
           |        "first attempt should see [x, i]")
           |      assert(ctx1.enclosingSource.contains("val x = 5"),
           |        "first attempt should see outer-body `val x = 5`")
           |      "definitelyMissingSym + 1"
           |    }
           |    if r1.isSuccess then r1.get
           |    else evalSafe[Int] { (ctx2: dotty.tools.repl.EvalContext) =>
           |      assert(ctx2.bindings.map(_.name).toSet == Set("r1", "x", "i"),
           |        "retry sits below `val r1` so its bindings include r1, x, i")
           |      assert(ctx2.enclosingSource.contains("val x = 5"),
           |        "retry should ALSO see outer-body `val x = 5`")
           |      "x + i"
           |    }.get${q3}""".stripMargin
      run(
        s"""|import dotty.tools.repl.{EvalContext, EvalResult}
            |def f(i: Int): Int =
            |  eval[Int] { (outerCtx: EvalContext) =>
            |    $innerBody
            |  }
            |println("f(10) = " + f(10))""".stripMargin
      )
      assertContains("f(10) = 15", storedOutput())
    }

  @Test def closureFormInNestedEvalSeesOuterBodyValAndChainedSource =
    // Models the user's nested-agent retry scenario:
    //   def f(i: Int) = eval(...)
    // where the outer eval's generated body itself declares `val x`
    // and then contains a nested eval. The runtime nested-eval
    // rewriter must:
    //   * inject BOTH `i` (outer) and `x` (outer-body local) as
    //     bindings on the inner call.
    //   * splice the outer body (with the inner call's location
    //     replaced by a marker) into the outer enclosingSource's
    //     marker slot, so the inner closure sees the full chain
    //     `def f(i: Int) = ({ val x = ...; __placeholder__ })`.
    //   * let the inner body reference both `x` and `i` so the
    //     wrapper signature has both as parameters.
    // String-interpolation in the inner body is avoided per the
    // existing test's comment about the pretty-printer round-trip.
    initially {
      val q3 = "\"\"\""
      val innerBody =
        s"""${q3}val x = 5
           |    eval[Int] { (innerCtx: dotty.tools.repl.EvalContext) =>
           |      assert(innerCtx.bindings.map(_.name).toSet == Set("x", "i"),
           |        "inner should capture both `x` and `i`")
           |      assert(innerCtx.enclosingSource.contains("def f(i: Int)"),
           |        "inner enclosingSource should still carry the def signature")
           |      assert(innerCtx.enclosingSource.contains("val x = 5"),
           |        "inner enclosingSource should include the outer body's `val x = 5`")
           |      assert(innerCtx.enclosingSource.contains("({ "),
           |        "inner enclosingSource should include the outer-body `({ ... })` wrapper")
           |      "x + i"
           |    }${q3}""".stripMargin
      run(
        s"""|import dotty.tools.repl.EvalContext
            |def f(i: Int): Int =
            |  eval[Int] { (outerCtx: EvalContext) =>
            |    assert(outerCtx.bindings.map(_.name).toSet == Set("i"))
            |    assert(outerCtx.enclosingSource.contains("def f(i: Int)"))
            |    $innerBody
            |  }
            |println("f(10) = " + f(10))""".stripMargin
      )
      assertContains("f(10) = 15", storedOutput())
    }

end DynamicEvalAgentApiTests

/** Tests for the `-Xrepl-eval-log-dir` compiler flag, which writes
 *  per-invocation log files for each `eval(...)` call. Each call
 *  produces:
 *
 *    - `eval_<timestamp>_enclosingSource.scala`: the enclosing
 *      top-level statement at the call site, with the eval call's
 *      location replaced by a placeholder.
 *    - `eval_<timestamp>_code.scala`: the body string the user
 *      submitted to `eval(...)`.
 *    - `eval_<timestamp>_error.scala`: only on a compile failure;
 *      carries the diagnostic text and the synthesised source the
 *      eval driver was trying to compile.
 */
class DynamicEvalLogTests extends ReplTest(
  ReplTest.defaultOptions ++ Array(
    "-Xrepl-eval-log-dir:" + DynamicEvalLogTests.tempDir.getAbsolutePath
  )
):
  import DynamicEvalLogTests.*

  @Test def writesEnclosingSourceAndCodeOnSuccess =
    initially {
      // Clean slate before this test.
      clearLogDir()
      run("""|def f(i: Int, j: Int): Int = eval[Int]("i + j")
             |f(10, 32)""".stripMargin)
      assertContains("val res0: Int = 42", storedOutput())
      val files = listLogs()
      val enc = files.find(_.endsWith("_enclosingSource.scala")).getOrElse(
        fail(s"missing enclosingSource log: $files").asInstanceOf[String])
      val code = files.find(_.endsWith("_code.scala")).getOrElse(
        fail(s"missing code log: $files").asInstanceOf[String])
      assertTrue(s"no error log expected on success: $files",
        files.forall(!_.endsWith("_error.scala")))
      val encContent = readLog(enc)
      val codeContent = readLog(code)
      assertTrue(s"enclosingSource should mention `def f`: $encContent",
        encContent.contains("def f(i: Int, j: Int): Int"))
      assertTrue(s"enclosingSource should contain placeholder: $encContent",
        encContent.contains("__evalBodyPlaceholder"))
      assertTrue(s"code should be the body string: $codeContent",
        codeContent.trim == "i + j")
    }

  @Test def writesErrorLogOnCompileFailure =
    initially {
      clearLogDir()
      run("""|import dotty.tools.repl.EvalCompileException
             |val r = try eval[Int]("undefinedSymbol + 1")
             |        catch case _: EvalCompileException => -1
             |r""".stripMargin)
      assertContains("val res0: Int = -1", storedOutput())
      val files = listLogs()
      val err = files.find(_.endsWith("_error.scala")).getOrElse(
        fail(s"missing error log: $files").asInstanceOf[String])
      val errContent = readLog(err)
      assertTrue(s"error log should mention diagnostic: $errContent",
        errContent.contains("Not found: undefinedSymbol"))
      assertTrue(s"error log should embed generated source: $errContent",
        errContent.contains("__EvalWrapper_"))
    }

end DynamicEvalLogTests

object DynamicEvalLogTests:
  private val tempDir: java.io.File =
    val d = java.io.File.createTempFile("eval-log-", "-test")
    d.delete()
    d.mkdirs()
    d.deleteOnExit()
    d

  def clearLogDir(): Unit =
    val files = tempDir.listFiles()
    if files != null then files.foreach(_.delete())

  def listLogs(): List[String] =
    val files = tempDir.listFiles()
    if files == null then Nil else files.map(_.getName).toList

  def readLog(name: String): String =
    val path = new java.io.File(tempDir, name).toPath
    new String(java.nio.file.Files.readAllBytes(path))
