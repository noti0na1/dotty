import scala.mutability._

class C:
  val x: C = ???

  @polyread def f1(): C @polyread = this

  @polyread def f2(s: String @polyread): C @polyread = x

def ff(x: C @polyread, y: C @polyread): C @polyread = ???

def ffpoly[T](x: T @polyread, y: T @polyread): T @polyread = ???

def test = {
  val c1: C = ???
  val c2: C @readonly = ???

  val c1x: C = c1.x
  val c2x1: C = c2.x // error
  val c2x2: C @readonly = c2.x
  val c2x3: C = c2.x.x // error
  val c2x4: C @readonly = c2.x.x

  val c1f1: C = c1.f1()
  def c1f1fn: () => C = c1.f1
  val c1f1appfn: C = c1f1fn()

  val c2f1_1: C = c2.f1() // error
  val c2f1_2: C @readonly = c2.f1()
  val c2f1_3: C = c2.f1().f1() // error

  val x1: String = ???
  val x2: String @readonly = ???

  val cf2_1: C = c2.f2(x1) // error
  val cf2_2: C @readonly = c2.f2(x1)
  val cf2_3: C = c2.f2(x2) // error
  val cf2_4: C @readonly = c2.f2(x2)

  def c2f2fn: String => C @readonly = c2.f2
  val c2f2appfn_1: C = c2f2fn(x1) // error
  val c2f2appfn_2: C = c2f2fn(x2) // error

  val ff1: C = ff(c1, c2) // error
  val ff2: C @readonly = ff(c1, c2)
  val ff3: C = ff(c1, c1)
  val ff4: C = ff(c2, c2) // error

  val ff1p: C = ffpoly(c1, c2) // error
  val ff2p: C @readonly = ffpoly(c1, c2)
  val ff3p: C = ffpoly(c1, c1)
  val ff4p: C = ffpoly(c2, c2) // error

  val xs: (C, C) @readonly = (c1, c2) // error
}
