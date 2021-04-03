// Test flow-typing when NotNullInfos are from cases

object MatchTest {

  def stringOrNullToString(s: String | Null): String = s match {
    case null => "null"
    // after the null case, s becomes non-nullable
    case _ => s
  }

  def stringOrNullToString2(s: String | Null): String = s match {
    case null => "null"
    // s in the case is new variable different from the paramter s
    // it has the original type: String | Null
    case s => s // error
  }
}
