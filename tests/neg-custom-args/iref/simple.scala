import scala.annotation.{readonly, polyread, mutable}

class Test {
  def f1 = {
    val x1: String = ???
    val x2: String @readonly = ???
    val x3: String @mutable = ???

    val y11: String = x1
    val y12: String = x2 // error
    val y13: String = x3

    val y21: String @readonly = x1
    val y22: String @readonly = x2
    val y23: String @readonly = x3

    val y31: String @mutable = x1
    val y32: String @mutable = x2 // error
    val y33: String @mutable = x3
  }

  def f2(x: String) = {
    val y1: String = x
    val y2: String @readonly = x
    val y3: String @mutable = x
  }

  def f3(x: String @readonly) = {
    val y1: String = x // error
    val y2: String @readonly = x
    val y3: String @mutable = x // error
  }

  def f4(x: String @mutable) = {
    val y1: String = x
    val y2: String @readonly = x
    val y3: String @mutable = x
  }
}