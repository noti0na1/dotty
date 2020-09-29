class S {
  def i[T](x: T): x.type = x

  def f[T <: AnyRef](x: T | Null): x.type & T = {
    if x != null then
      i[x.type](x)
    else
      throw Exception()
  }
}