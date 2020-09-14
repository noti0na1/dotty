def f1 = {
  val s: String | Null = ""
  val s1: String = s.trim()
  val s2 = s.concat(s1)
}

import java.util.List
import java.util.Arrays

def f2 = {
  val ss: Array[String] = Array()
  val ss1: List[String] = Arrays.asList(ss: _*)
  val ss2: List[String | Null] = Arrays.asList(ss: _*)
  val ss3: Array[String] = ss2.toArray(ss)
  val ss4: Array[String | Null] = ss1.toArray(ss)
}