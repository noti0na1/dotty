package scala.mutability

import scala.annotation.StaticAnnotation

final class mut[T >: Mutable <: Readonly] extends StaticAnnotation
