package TypeSystem

import TypeSystem.BinOpRules.*

enum BuiltInType(val name: String) {
  case IntegerT extends BuiltInType("integer")
  case RealT extends BuiltInType("real")
  case BooleanT extends BuiltInType("boolean")
  case StringT extends BuiltInType("string")
}