package TypeSystem


type BinOp =
  ArithmeticBinOps
    | RealDivBinOp.type
    | IntDivBinOp.type
    | EqualityBinOps
    | ComparisonBinOps
    | LogicBinOps

enum ArithmeticBinOps() {
  case Add extends ArithmeticBinOps()
  case Sub extends ArithmeticBinOps()
  case Mul extends ArithmeticBinOps()
}

case object RealDivBinOp;

case object IntDivBinOp;


enum EqualityBinOps {
  case Equal extends EqualityBinOps()
  case NotEqual extends EqualityBinOps()
}

enum ComparisonBinOps {
  case Less extends ComparisonBinOps()
  case LessOrEqual extends ComparisonBinOps()
  case Greater extends ComparisonBinOps()
  case GreaterOrEqual extends ComparisonBinOps()
}

enum LogicBinOps {
  case And extends LogicBinOps()
  case Or extends LogicBinOps()
  case Xor extends LogicBinOps()
}

