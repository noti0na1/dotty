import scala.annotation.{readonly, polyread, mutable}

case class F[A](val underlying: Array[A] @readonly, val x: A)
