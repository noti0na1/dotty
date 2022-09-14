package dotty.tools
package dotc
package mutability

enum MutabilityQualifier(val d: Int) extends Ordered[MutabilityQualifier]:

  case Mutable extends MutabilityQualifier(0)

  case Polyread extends MutabilityQualifier(1)

  case Readonly extends MutabilityQualifier(2)

  def compare(that: MutabilityQualifier) =  this.d - that.d
