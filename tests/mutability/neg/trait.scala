import scala.annotation.{readonly, polyread, mutable}

class Trans[A, C[A]]:
  @polyread
  def ++[B](xs: List[A] @polyread): C[B] @polyread = ???

class ArrayTrans[A] extends Trans[A, Array]:
  @polyread
  override def ++[B](xs: List[A] @polyread): Array[B] @polyread =
    super.++(xs)

trait ListTrans[A] extends Trans[A, ListTrans]:

  def f: String

  @polyread
  override def ++[B](xs: List[A] @polyread): ListTrans[B] @polyread =
    super.++(xs)
