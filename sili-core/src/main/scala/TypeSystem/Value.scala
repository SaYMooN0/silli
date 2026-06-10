package TypeSystem

import SemanticAnalyzer.TypeSymbol

enum Value(val t: BuiltInType) {
  case RealValue(v: Double) extends Value(BuiltInType.RealT)
  case IntegerValue(v: Int) extends Value(BuiltInType.IntegerT)
  case StringValue(v: String) extends Value(BuiltInType.StringT)
  case BooleanValue(v: Boolean) extends Value(BuiltInType.BooleanT)
}

def coerceValueTo(value: Value, targetType: TypeSymbol): Value = {
  (value, targetType) match {
    case (Value.IntegerValue(v), TypeSymbol.RealSym) => Value.RealValue(v.toDouble)
    case _                                           => value
  }
}