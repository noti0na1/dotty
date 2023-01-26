import scala.annotation.{readonly, polyread, mutable}

class C:

  val s: C = null

  val t: C @readonly = null

  var i: Int = 0

  // def f1(c: C) =
  //   c.i = 1

  //   c.s.i = 2
  //   c.t.i = 2 // error

  //   c.s.s.i = 3
  //   c.s.t.i = 3 // error

  //   c.t.s.i = 3 // error
  //   c.t.t.i = 3 // error

  def f2(c: C @readonly) =
    c.i = 1 // error

    c.s.i = 2 // error
    c.t.i = 2 // error

    c.s.s.i = 3 // error
    c.s.t.i = 3 // error

    c.t.s.i = 3 // error
    c.t.t.i = 3 // error

  def f3 =
    i = 1

    s.i = 2
    t.i = 2 // error

    s.s.i = 3
    s.t.i = 3 // error

  @polyread def f4 =
    i = 1 // error

    s.i = 2 // error
    t.i = 2 // error

    s.s.i = 3 // error
    s.t.i = 3 // error

  @readonly def f5 =
    i = 1 // error

    s.i = 2 // error
    t.i = 2 // error

    s.s.i = 3 // error
    s.t.i = 3 // error

  def f6 =
    val x: C @readonly = ???
    val y: x.type = x
    y.i = 1 // error

  type ReadonlyC = C @readonly
  type PolyreadOf[T] = T @polyread
  type ReadonlyOf[T] = T @readonly

  def f7 =
    val x: ReadonlyC = ???
    x.i = 1 // error

    val y: PolyreadOf[C] = ???
    y.i = 1 // error

    val z: ReadonlyOf[C] = ???
    z.i = 1 // error

  // TODO: add match type
