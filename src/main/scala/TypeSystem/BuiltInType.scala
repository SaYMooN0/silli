package TypeSystem

import TypeSystem.BinOpRules.*

enum BuiltInType(val name: String) {
  case IntegerT extends BuiltInType("integer")
  case RealT extends BuiltInType("real")
  case BooleanT extends BuiltInType("boolean")
  case StringT extends BuiltInType("string")
}

def supportedBinOpsFor(t: BuiltInType): BinOpRulesMap = t match {
  case BuiltInType.IntegerT => IntegerSupportedBinOps.rules;
  case BuiltInType.RealT => RealSupportedBinOps.rules;
  case BuiltInType.BooleanT => BooleanSupportedBinOps.rules;
  case BuiltInType.StringT => StringSupportedBinOps.rules;
}
def inferBinOpResultType(leftType: BuiltInType, op: BinOp, rightType: BuiltInType): Option[BuiltInType] =
  supportedBinOpsFor(leftType)
    .get(rightType)
    .flatMap(_.get(op))