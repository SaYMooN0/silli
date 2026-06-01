package TypeSystem

enum UnOp {
  case Plus extends UnOp()
  case Minus extends UnOp()
  case Not extends UnOp()
}

object UnOpRules {
  private final case class Rule(
                                 inputType: BuiltInType,
                                 resultType: BuiltInType,
                                 apply: Value => Either[OpEvalErr, Value]
                               )

  private def intToIntRule(f: Int => Int): Rule =
    Rule(
      BuiltInType.IntegerT,
      BuiltInType.IntegerT,
      apply = {
        case Value.IntegerValue(v) =>
          try Right(Value.IntegerValue(f(v)))
          catch {
            case _: ArithmeticException => Left(OpEvalErr.IntegerOverflow)
          }

        case _ =>
          Left(OpEvalErr.UnsupportedOperation)
      }
    )

  private def realToRealRule(f: Double => Double): Rule =
    Rule(
      BuiltInType.RealT,
      BuiltInType.RealT,
      apply = {
        case Value.RealValue(v) =>
          checkedRealResult(f(v))

        case _ =>
          Left(OpEvalErr.UnsupportedOperation)
      }
    )

  private def boolToBoolRule(f: Boolean => Boolean): Rule =
    Rule(
      BuiltInType.BooleanT,
      BuiltInType.BooleanT,
      apply = {
        case Value.BooleanValue(v) =>
          Right(Value.BooleanValue(f(v)))

        case _ =>
          Left(OpEvalErr.UnsupportedOperation)
      }
    )

  private val rules: Map[(UnOp, BuiltInType), Rule] = Map(
    (UnOp.Plus, BuiltInType.IntegerT) -> intToIntRule(identity),
    (UnOp.Minus, BuiltInType.IntegerT) -> intToIntRule(v => java.lang.Math.negateExact(v)),

    (UnOp.Plus, BuiltInType.RealT) -> realToRealRule(identity),
    (UnOp.Minus, BuiltInType.RealT) -> realToRealRule(v => -v),

    (UnOp.Not, BuiltInType.BooleanT) -> boolToBoolRule(v => !v)
  )

  def inferResultType(innerType: BuiltInType, op: UnOp): Option[BuiltInType] =
    rules
      .get((op, innerType))
      .map(_.resultType)

  def applyOp(op: UnOp, value: Value): Either[OpEvalErr, Value] =
    rules
      .get((op, value.t)) match {
      case Some(rule) => rule.apply(value)
      case None => Left(OpEvalErr.UnsupportedOperation)
    }

  private def checkedRealResult(value: Double): Either[OpEvalErr, Value] =
    if java.lang.Double.isNaN(value) then Left(OpEvalErr.InvalidRealResult)
    else if java.lang.Double.isInfinite(value) then Left(OpEvalErr.RealOverflow)
    else Right(Value.RealValue(value))
}