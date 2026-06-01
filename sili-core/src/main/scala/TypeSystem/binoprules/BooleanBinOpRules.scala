package TypeSystem.binoprules

import TypeSystem.*
import TypeSystem.BinOpRules.BinOpRulesMap

private[TypeSystem] object BooleanBinOpRules {
  val rules: BinOpRulesMap = Map(
    BuiltInType.BooleanT -> Map(
      LogicBinOps.And -> boolBoolToBool(_ && _),
      LogicBinOps.Or -> boolBoolToBool(_ || _),
      LogicBinOps.Xor -> boolBoolToBool(_ ^ _),

      EqualityBinOps.Equal -> boolBoolToBool(_ == _),
      EqualityBinOps.NotEqual -> boolBoolToBool(_ != _)
    )
  )

  private def boolBoolToBool(f: (Boolean, Boolean) => Boolean): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.BooleanT, {
      case (Value.BooleanValue(a), Value.BooleanValue(b)) => Right(Value.BooleanValue(f(a, b)))
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })
}