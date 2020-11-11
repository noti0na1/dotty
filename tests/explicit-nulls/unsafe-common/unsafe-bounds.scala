// See explicit-nulls/neg/bounds.scala

val x1: String = ???
val x2: String | Null = ???

def f1[T >: Null <: AnyRef | Null](x: T): T = x
def f2[T >: Null <: AnyRef](x: T): T = x

def nullOf[T >: Null <: AnyRef]: T = null

def g = {
  f1(x1)
  f1(x2)

  f2[String](x2) // error

  f2(x1) // error
  f2(x2) // error

  val n1: String = nullOf // error
  val n2: String | Null = nullOf // ok, Null is inferred
}
