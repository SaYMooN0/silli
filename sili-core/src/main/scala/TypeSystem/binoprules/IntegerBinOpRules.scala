package TypeSystem.binoprules

import TypeSystem.*
import TypeSystem.BinOpRules.BinOpRulesMap

private[TypeSystem] object IntegerBinOpRules {
  val rules: BinOpRulesMap = Map(
    BuiltInType.IntegerT -> Map(
      ArithmeticBinOps.Add -> checkedIntIntToInt((a, b) => java.lang.Math.addExact(a, b)),
      ArithmeticBinOps.Sub -> checkedIntIntToInt((a, b) => java.lang.Math.subtractExact(a, b)),
      ArithmeticBinOps.Mul -> checkedIntIntToInt((a, b) => java.lang.Math.multiplyExact(a, b)),
      IntDivBinOp -> checkedIntDiv,
      IntDivBinOp -> checkedIntIntToInt((a, b) => a % b),
      RealDivBinOp -> intRealDivToReal,

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
      RealDivBinOp -> intRealDivToReal,

      EqualityBinOps.Equal -> intRealToBool((a, b) => a.toDouble == b),
      EqualityBinOps.NotEqual -> intRealToBool((a, b) => a.toDouble != b),
      ComparisonBinOps.Less -> intRealToBool((a, b) => a.toDouble < b),
      ComparisonBinOps.LessOrEqual -> intRealToBool((a, b) => a.toDouble <= b),
      ComparisonBinOps.Greater -> intRealToBool((a, b) => a.toDouble > b),
      ComparisonBinOps.GreaterOrEqual -> intRealToBool((a, b) => a.toDouble >= b)
    )
  )

  private def checkedIntIntToInt(f: (Int, Int) => Int): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.IntegerT, {
      case (Value.IntegerValue(a), Value.IntegerValue(b)) =>
        try Right(Value.IntegerValue(f(a, b)))
        catch {
          case _: ArithmeticException => Left(OpEvalErr.IntegerOverflow)
        }

      case _ => Left(OpEvalErr.UnsupportedOperation)
    })

  private def checkedIntDiv: BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.IntegerT, {
      case (Value.IntegerValue(_), Value.IntegerValue(0))             => Left(OpEvalErr.DivisionByZero)
      case (Value.IntegerValue(Int.MinValue), Value.IntegerValue(-1)) => Left(OpEvalErr.IntegerOverflow)
      case (Value.IntegerValue(a), Value.IntegerValue(b))             => Right(Value.IntegerValue(a / b))
      case _                                                          => Left(OpEvalErr.UnsupportedOperation)
    })

  private def intIntToBool(f: (Int, Int) => Boolean): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.BooleanT, {
      case (Value.IntegerValue(a), Value.IntegerValue(b)) => Right(Value.BooleanValue(f(a, b)))
      case _                                              => Left(OpEvalErr.UnsupportedOperation)
    })

  private def intRealToReal(f: (Int, Double) => Double): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.RealT, {
      case (Value.IntegerValue(a), Value.RealValue(b)) => checkedRealResult(f(a, b))
      case _                                           => Left(OpEvalErr.UnsupportedOperation)
    })

  private def intRealDivToReal: BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.RealT, {
      case (Value.IntegerValue(_), Value.RealValue(0.0))  => Left(OpEvalErr.DivisionByZero)
      case (Value.IntegerValue(a), Value.RealValue(b))    => checkedRealResult(a.toDouble / b)
      case (Value.IntegerValue(a), Value.IntegerValue(b)) => checkedRealResult(a.toDouble / b.toDouble)
      case _                                              => Left(OpEvalErr.UnsupportedOperation)
    })

  private def intRealToBool(f: (Int, Double) => Boolean): BinOpRules.Rule =
    BinOpRules.Rule(BuiltInType.BooleanT, {
      case (Value.IntegerValue(a), Value.RealValue(b)) => Right(Value.BooleanValue(f(a, b)))
      case _                                           => Left(OpEvalErr.UnsupportedOperation)
    })

  private def checkedRealResult(value: Double): Either[OpEvalErr, Value] =
    if java.lang.Double.isNaN(value) then Left(OpEvalErr.InvalidRealResult)
    else if java.lang.Double.isInfinite(value) then Left(OpEvalErr.RealOverflow)
    else Right(Value.RealValue(value))
}