import java.nio.file.Paths

def f = {
  Paths.get("")
  Paths.get("", null)
  Paths.get("", "")
  Paths.get("", "", null)

  val x1: String = ???
  val x2: String | Null = ???

  Paths.get("", x1)
  Paths.get("", x2)

  val arg1: Array[String] = ???
  val arg2: Array[String | Null] = ???
  val arg3: Array[String] | Null = ???
  val arg4: Array[String | Null] | Null = ???

  Paths.get("", arg1: _*)
  Paths.get("", arg2: _*)
  Paths.get("", arg3: _*) // error
  Paths.get("", arg4: _*) // error
}