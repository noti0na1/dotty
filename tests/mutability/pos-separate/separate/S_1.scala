import scala.annotation.{readonly, polyread, mutable}

object Test:
  trait A
  case object B extends A
  def f(i: Int, a: A = B): A = a

