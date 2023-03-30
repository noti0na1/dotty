import scala.mutability._

class C:
  def f1(x: C): C = ???
  @polyread def f2(x: C @polyread): C @polyread = ???
  @readonly def f3(x: C @readonly): C @readonly = ???

class D extends C:
  override def f1(x: C): C = ???
  @polyread override def f2(x: C @polyread): C @polyread = ???
  @readonly override def f3(x: C @readonly): C @readonly = ???

class E extends C:
  override def f1(x: C @readonly): C = ??? // error
  @polyread override def f2(x: C @polyread): C = ???

class F extends C:
  @readonly override def f1(x: C): C = ??? // error
  override def f2(x: C @polyread): C = ??? // error
