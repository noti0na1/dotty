import scala.mutability._

case class F[A](val underlying: Array[A] @readonly, val x: A)

@readonly case class G[A](val underlying: Array[A] @readonly, val x: A @readonly)

@readonly case class H[A](underlying: Array[A] @readonly, x: A) // error
