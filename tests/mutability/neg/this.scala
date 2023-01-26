import scala.annotation.{readonly, polyread, mutable}

class C:

  var i: Int = 0

  def f1(): Unit =
    this.i = 1
    val c: C = this
    c.i = 2
    f1()
    f2()
    f3()

  @polyread def f2(): Unit =
    this.i = 1 // error
    val c: C = this // error
    val d: C @polyread = this
    f1() // error
    f2()
    f3()

  @readonly def f3(): Unit =
    this.i = 1 // error
    val c: C = this // error
    val d: C @polyread = this // error
    val e: C @readonly = this
    f1() // error
    f2()
    f3()

  // TODO: add nested this

class D extends C
