package scala.mutability

import scala.annotation.StaticAnnotation

final class mut[M >: mutable <: readonly] extends StaticAnnotation
