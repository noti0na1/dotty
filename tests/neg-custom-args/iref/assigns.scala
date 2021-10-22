import scala.annotation.{readonly, polyread, mutable}

class C {
  val x: C @mutable = null
  val y: C @polyread = null
  val z: C @readonly = null
  var i = 0

  def f1(c: C @mutable) =
    c.i = 1

    c.x.i = 2
    c.y.i = 2
    c.z.i = 2 // error

    c.x.x.i = 3
    c.x.y.i = 3
    c.x.z.i = 3 // error

    c.y.x.i = 3
    c.y.y.i = 3
    c.y.z.i = 3 // error

    c.z.x.i = 3
    c.z.y.i = 3 // error
    c.z.z.i = 3 // error

  def f2(c: C @readonly) =
    c.i = 1 // error

    c.x.i = 2
    c.y.i = 2 // error
    c.z.i = 2 // error

    c.x.x.i = 3
    c.x.y.i = 3
    c.x.z.i = 3 // error

    c.y.x.i = 3
    c.y.y.i = 3 // error
    c.y.z.i = 3 // error

    c.z.x.i = 3
    c.z.y.i = 3 // error
    c.z.z.i = 3 // error

  @mutable def f3 =
    i = 1

    x.i = 2
    y.i = 2
    z.i = 2 // error

    x.x.i = 3
    x.y.i = 3
    x.z.i = 3 // error

    y.x.i = 3
    y.y.i = 3
    y.z.i = 3 // error

    z.x.i = 3
    z.y.i = 3 // error
    z.z.i = 3 // error

  @readonly def f4 =
    i = 1 // error

    x.i = 2
    y.i = 2 // error
    z.i = 2 // error

    x.x.i = 3
    x.y.i = 3
    x.z.i = 3 // error

    y.x.i = 3
    y.y.i = 3 // error
    y.z.i = 3 // error

    z.x.i = 3
    z.y.i = 3 // error
    z.z.i = 3 // error

  // // @polyread def f5

  def f6 =
    val x: C @readonly = ???
    val y: x.type = x
    y.i = 1 // error

  type ReadOnlyC = C @readonly

  def f7 =
    val x: ReadOnlyC = ???
    x.i = 1 // error

  // TODO: add match type
}