package TypeSystem.binoprules

import TypeSystem.*
import TypeSystem.BinOpRules.BinOpRulesMap

private[TypeSystem] object StringBinOpRules {
  val rules: BinOpRulesMap = Map(
    BuiltInType.StringT -> Map(
      ArithmeticBinOps.Add -> stringStringToString(_ + _),

      EqualityBinOps.Equal -> stringStringToBool(_ == _),
      EqualityBinOps.NotEqual -> stringStringToBool(_ != _)
    )
  )

  private def stringStringToString(f: (String, String) => String): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.StringT, {
      case (Value.StringValue(a), Value.StringValue(b)) => Right(Value.StringValue(f(a, b)))
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })

  private def stringStringToBool(f: (String, String) => Boolean): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.BooleanT, {
      case (Value.StringValue(a), Value.StringValue(b)) => Right(Value.BooleanValue(f(a, b)))
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })
}