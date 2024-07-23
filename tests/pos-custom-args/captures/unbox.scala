import language.`3.2`
type Proc = () => Unit

val xs: List[Proc] = ???

val x = xs.head // error

val xs2: List[() ->{xs*} Unit] = xs

val x2: () ->{xs*} Unit = xs2.head
