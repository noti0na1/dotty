import scala.annotation.{readonly, polyread, mutable}

class TestVars {
  def f1 = {
    var x1: String = ???
    var x2: String @readonly = ???
    var x3: String @mutable = ???

    var y11: String = x1
    var y12: String = x2 // error
    var y13: String = x3

    var y21: String @readonly = x1
    var y22: String @readonly = x2
    var y23: String @readonly = x3

    var y31: String @mutable = x1
    var y32: String @mutable = x2 // error
    var y33: String @mutable = x3
  }

  def f2(x: String) = {
    var y1: String = x
    var y2: String @readonly = x
    var y3: String @mutable = x
  }

  def f3(x: String @readonly) = {
    var y1: String = x // error
    var y2: String @readonly = x
    var y3: String @mutable = x // error
  }

  def f4(x: String @mutable) = {
    var y1: String = x
    var y2: String @readonly = x
    var y3: String @mutable = x
  }
}