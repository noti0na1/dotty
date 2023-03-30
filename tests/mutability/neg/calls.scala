import scala.mutability._

class A:

  @readonly
  def a1: Int = 0

  @mutable
  def a2: Int = 0

  @readonly
  def b(f: Int => Int): Int = f(a1)

class B:

  var z: Int = 0

  def f1(a: A @readonly) =
    val x1 = a.a1
    val x2 = a.a2 // error

  def f2(a: A @mutable) =
    val x1 = a.a1
    val x2 = a.a2

  @readonly
  def f3(a: A @readonly): Int =
    a.b(i => i + z)

  @readonly
  def f4(a: A @readonly): Int =
    a.b(i => i + g) // error

  @readonly
  def g1 =
    z = 1 // error

  @mutable
  def g2 =
    z = 1

  @mutable
  def g: Int =
    1

  @readonly
  def h1 =
    val x = g // error

  @mutable
  def h2 =
    val x = g

  @mutable
  def h3 = g

  @mutable
  def h4 = this.g

  @mutable
  def h5 = B.this.g

  @readonly
  def k1: Int =
    def l: Int = this.g // error
    l

  @readonly
  def k2: Int =
    class C:
      def l: Int = g // error
    (new C).l

class ExtTest:

  extension (a: A @readonly)

    def getV1: Int = a.a1

    def getV2: Int = a.a2 // error

  extension (a: A)

    def getV3: Int = a.a1
