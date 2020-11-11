// The bound check is in PostTyper, hence we need a seperate test

// See explicit-nulls/neg/bounds.scala

val x1: String = ???
val x2: String | Null = ???

def f2[T >: Null <: AnyRef](x: T): T = x

def nullOf[T >: Null <: AnyRef]: T = null

def g = {
  f2[String](x1) // error
  f2[String | Null](x1) // error
  f2[String | Null](x2) // error
}
