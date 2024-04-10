//> using options  -Wunused:imports

def test(list: List[Int]): Int =
  import list.{length => len} // warn
  import list.{head => first}
  first + list.length