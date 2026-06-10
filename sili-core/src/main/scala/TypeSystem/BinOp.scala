package TypeSystem


type BinOp =
  ArithmeticBinOps
    | RealDivBinOp.type
    | IntDivBinOp.type
    | ModBinOp.type
    | EqualityBinOps
    | ComparisonBinOps
    | LogicBinOps

enum ArithmeticBinOps() {
  case Add extends ArithmeticBinOps()
  case Sub extends ArithmeticBinOps()
  case Mul extends ArithmeticBinOps()
}

case object RealDivBinOp

case object IntDivBinOp

case object ModBinOp


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


object BinOpRules {
  private[TypeSystem] final case class Rule(resultType: BuiltInType, apply: (Value, Value) => Either[OpEvalErr, Value])

  private[TypeSystem] type BinOpRulesMap = Map[BuiltInType, Map[BinOp, Rule]]

  private val rulesByLeftType: Map[BuiltInType, BinOpRulesMap] = Map(
    BuiltInType.IntegerT -> binoprules.IntegerBinOpRules.rules,
    BuiltInType.RealT -> binoprules.RealBinOpRules.rules,
    BuiltInType.BooleanT -> binoprules.BooleanBinOpRules.rules,
    BuiltInType.StringT -> binoprules.StringBinOpRules.rules
  )

  private def supportedFor(leftType: BuiltInType): BinOpRulesMap =
    rulesByLeftType.getOrElse(leftType, Map.empty)

  def inferResultType(leftType: BuiltInType, op: BinOp, rightType: BuiltInType): Option[BuiltInType] = {
    supportedFor(leftType)
      .get(rightType)
      .flatMap(_.get(op))
      .map(_.resultType)
  }

  def applyOp(left: Value, op: BinOp, right: Value): Either[OpEvalErr, Value] = {
    supportedFor(left.t)
      .get(right.t)
      .flatMap(_.get(op)) match {
      case Some(rule) => rule.apply(left, right)
      case None => Left(OpEvalErr.UnsupportedOperation)
    }
  }
}