import scala.annotation.{readonly, polyread, mutable}
import scala.annotation.targetName

// TODO: add targetName automatically for overloaded methods?

class C:
  def f(x: C): Unit = ???
  @targetName("f_readonly")
  def f(x: C @readonly): Unit = ???
