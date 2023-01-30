import scala.annotation.{readonly, polyread, mutable}

case class C(i: Int):
  var j: Int = 0

def getC(i: Int): C @readonly = C(i)

def f1(i: Int): C @readonly = i match
  case 0 => C(0)
  case 1 => C(1)
  case _ => C(-1)

// TODO: currently we need to annotate the case result explicitly
// because in typer, the tree is given a wrong type
def f2(i: Int): C @readonly = i match
  case 0 => (C(0): C @readonly)
  case 1 => (C(1): C @readonly)
  case _ => (getC(-1): C @readonly)

def g1(c: C @readonly) = c match
  case x: C @readonly => x.i

// TODO: this should be an error,
// but we currently don't have a way to check the case mutability
// def g2(c: C @readonly) = c match
//   case x: C => x.i // error
