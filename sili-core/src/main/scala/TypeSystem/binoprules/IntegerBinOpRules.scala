package TypeSystem.binoprules

import TypeSystem.*
import TypeSystem.BinOpRules.BinOpRulesMap

private[TypeSystem] object IntegerBinOpRules {
  val rules: BinOpRulesMap = Map(
    BuiltInType.IntegerT -> Map(
      ArithmeticBinOps.Add -> intIntToInt(_ + _),
      ArithmeticBinOps.Sub -> intIntToInt(_ - _),
      ArithmeticBinOps.Mul -> intIntToInt(_ * _),
      IntDivBinOp -> intIntToInt(_ / _),

      EqualityBinOps.Equal -> intIntToBool(_ == _),
      EqualityBinOps.NotEqual -> intIntToBool(_ != _),
      ComparisonBinOps.Less -> intIntToBool(_ < _),
      ComparisonBinOps.LessOrEqual -> intIntToBool(_ <= _),
      ComparisonBinOps.Greater -> intIntToBool(_ > _),
      ComparisonBinOps.GreaterOrEqual -> intIntToBool(_ >= _)
    ),

    BuiltInType.RealT -> Map(
      ArithmeticBinOps.Add -> intRealToReal((a, b) => a.toDouble + b),
      ArithmeticBinOps.Sub -> intRealToReal((a, b) => a.toDouble - b),
      ArithmeticBinOps.Mul -> intRealToReal((a, b) => a.toDouble * b),
      RealDivBinOp -> intRealToReal((a, b) => a.toDouble / b),

      EqualityBinOps.Equal -> intRealToBool((a, b) => a.toDouble == b),
      EqualityBinOps.NotEqual -> intRealToBool((a, b) => a.toDouble != b),
      ComparisonBinOps.Less -> intRealToBool((a, b) => a.toDouble < b),
      ComparisonBinOps.LessOrEqual -> intRealToBool((a, b) => a.toDouble <= b),
      ComparisonBinOps.Greater -> intRealToBool((a, b) => a.toDouble > b),
      ComparisonBinOps.GreaterOrEqual -> intRealToBool((a, b) => a.toDouble >= b)
    )
  )
  private def intIntToInt(f: (Int, Int) => Int): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.IntegerT, {
      case (Value.IntegerValue(a), Value.IntegerValue(b)) => Some(Value.IntegerValue(f(a, b)))
      case _ => None
    })

  private def intIntToBool(f: (Int, Int) => Boolean): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.BooleanT, {
      case (Value.IntegerValue(a), Value.IntegerValue(b)) => Some(Value.BooleanValue(f(a, b)))
      case _ => None
    })

  private def intRealToReal(f: (Int, Double) => Double): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.RealT, {
      case (Value.IntegerValue(a), Value.RealValue(b)) =>
        Some(Value.RealValue(f(a, b)))

      case _ => None
    })

  private def intRealToBool(f: (Int, Double) => Boolean): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.BooleanT, {
      case (Value.IntegerValue(a), Value.RealValue(b)) => Some(Value.BooleanValue(f(a, b)))
      case _ => None
    })
}