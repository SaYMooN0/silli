package TypeSystem

enum OpEvalErr {
  case UnsupportedOperation
  case DivisionByZero
  case IntegerOverflow
  case RealOverflow
  case InvalidRealResult
}
