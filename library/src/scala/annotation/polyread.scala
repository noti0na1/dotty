package scala.annotation

import java.lang.annotation.ElementType
import java.lang.annotation.Target

@Target(Array(ElementType.TYPE, ElementType.METHOD))
final class polyread extends StaticAnnotation
