package TypeSystem.binoprules

import TypeSystem.*
import TypeSystem.BinOpRules.BinOpRulesMap

private[TypeSystem] object RealBinOpRules {
  val rules: BinOpRulesMap = Map(
    BuiltInType.IntegerT -> Map(
      ArithmeticBinOps.Add -> realIntToReal((a, b) => a + b.toDouble),
      ArithmeticBinOps.Sub -> realIntToReal((a, b) => a - b.toDouble),
      ArithmeticBinOps.Mul -> realIntToReal((a, b) => a * b.toDouble),
      RealDivBinOp -> realIntDivToReal,

      EqualityBinOps.Equal -> realIntToBool((a, b) => a == b.toDouble),
      EqualityBinOps.NotEqual -> realIntToBool((a, b) => a != b.toDouble),
      ComparisonBinOps.Less -> realIntToBool((a, b) => a < b.toDouble),
      ComparisonBinOps.LessOrEqual -> realIntToBool((a, b) => a <= b.toDouble),
      ComparisonBinOps.Greater -> realIntToBool((a, b) => a > b.toDouble),
      ComparisonBinOps.GreaterOrEqual -> realIntToBool((a, b) => a >= b.toDouble)
    ),

    BuiltInType.RealT -> Map(
      ArithmeticBinOps.Add -> realRealToReal(_ + _),
      ArithmeticBinOps.Sub -> realRealToReal(_ - _),
      ArithmeticBinOps.Mul -> realRealToReal(_ * _),
      RealDivBinOp -> realRealDivToReal,

      EqualityBinOps.Equal -> realRealToBool(_ == _),
      EqualityBinOps.NotEqual -> realRealToBool(_ != _),
      ComparisonBinOps.Less -> realRealToBool(_ < _),
      ComparisonBinOps.LessOrEqual -> realRealToBool(_ <= _),
      ComparisonBinOps.Greater -> realRealToBool(_ > _),
      ComparisonBinOps.GreaterOrEqual -> realRealToBool(_ >= _)
    )
  )

  private def realIntToReal(f: (Double, Int) => Double): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.RealT, {
      case (Value.RealValue(a), Value.IntegerValue(b)) => checkedRealResult(f(a, b))
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })

  private def realIntDivToReal: BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.RealT, {
      case (Value.RealValue(_), Value.IntegerValue(0)) => Left(OpEvalErr.DivisionByZero)
      case (Value.RealValue(a), Value.IntegerValue(b)) => checkedRealResult(a / b.toDouble)
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })

  private def realIntToBool(f: (Double, Int) => Boolean): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.BooleanT, {
      case (Value.RealValue(a), Value.IntegerValue(b)) => Right(Value.BooleanValue(f(a, b)))
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })

  private def realRealToReal(f: (Double, Double) => Double): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.RealT, {
      case (Value.RealValue(a), Value.RealValue(b)) => checkedRealResult(f(a, b))
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })

  private def realRealDivToReal: BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.RealT, {
      case (Value.RealValue(_), Value.RealValue(0.0)) => Left(OpEvalErr.DivisionByZero)
      case (Value.RealValue(a), Value.RealValue(b)) => checkedRealResult(a / b)
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })

  private def realRealToBool(f: (Double, Double) => Boolean): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.BooleanT, {
      case (Value.RealValue(a), Value.RealValue(b)) => Right(Value.BooleanValue(f(a, b)))
      case _ => Left(OpEvalErr.UnsupportedOperation)
    })

  private def checkedRealResult(value: Double): Either[OpEvalErr, Value] =
    if java.lang.Double.isNaN(value) then Left(OpEvalErr.InvalidRealResult)
    else if java.lang.Double.isInfinite(value) then Left(OpEvalErr.RealOverflow)
    else Right(Value.RealValue(value))
}