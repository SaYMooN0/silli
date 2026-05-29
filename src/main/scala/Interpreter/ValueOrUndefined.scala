package Interpreter

import TypeSystem.Value

enum ValueOrUndefined {
  case Undefined
  case Value(value: TypeSystem.Value)
}