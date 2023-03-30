import scala.mutability._

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

  @readonly def f4(): AnyRef =
    class E:
      def h1(): Unit = ???
      @readonly def h2(): Unit =
        i = 1 // error
        val c: C = C.this // error
        val d: C @polyread = C.this // error
        val e: C @readonly = C.this
        f1() // error
        f2()
        f3()
        h1() // error
        h2()
    new E

class D extends C:

  override def f1(): Unit =
    i = 1
    super.f1()
    super.f2()
    super.f3()

  @polyread override def f2(): Unit =
    i = 1 // error
    super.f1() // error
    super.f2()
    super.f3()

  @readonly override def f3(): Unit =
    i = 1 // error
    super.f1() // error
    super.f2()
    super.f3()
