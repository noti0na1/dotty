class S {
  given Conversion[String, Array[String]] = _ => ???

  def f = {
    val s: String | Null = ???

    val x: String = s // error
    val xs: Array[String | Null] = s // error

    {
      import scala.language.unsafeNulls
      // ensure the previous search cache is not used here
      val y: String = s
      val ys: Array[String | Null] = s
    }

    val z: String = s // error
    val zs: Array[String | Null] = s // error
  }
}