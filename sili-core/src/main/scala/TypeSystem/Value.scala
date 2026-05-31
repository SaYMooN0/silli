package TypeSystem

enum Value(val t: BuiltInType) {
  case RealValue(v: Double) extends Value(BuiltInType.RealT)
  case IntegerValue(v: Int) extends Value(BuiltInType.IntegerT)
  case StringValue(v: String) extends Value(BuiltInType.StringT)
  case BooleanValue(v: Boolean) extends Value(BuiltInType.BooleanT)
}