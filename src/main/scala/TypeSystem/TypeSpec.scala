package TypeSystem

enum TypeSpec(val name: String) {
  case IntegerT extends TypeSpec("integer")
  case RealT extends TypeSpec("real")
  case BooleanT extends TypeSpec("boolean")
  case StringT extends TypeSpec("string")
}