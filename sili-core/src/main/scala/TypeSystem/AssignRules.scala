package TypeSystem

object AssignRules {
  def canBeAssigned(targetType: BuiltInType, receivedType: BuiltInType): Boolean = (targetType, receivedType) match {
    case (BuiltInType.RealT, BuiltInType.IntegerT) => true
    case (t, r) => t == r
  }
}
