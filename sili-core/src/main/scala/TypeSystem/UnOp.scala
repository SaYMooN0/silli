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
                         apply: Value => Option[Value]
                       )

  private def intToIntRule(f: Int => Value): Rule =
    Rule(
      BuiltInType.IntegerT, BuiltInType.IntegerT,
      apply = {
        case Value.IntegerValue(v) => Some(f(v))
        case _ => None
      }
    )

  private def realToRealRule(f: Double => Value): Rule =
    Rule(
      BuiltInType.RealT, BuiltInType.RealT,
      apply = {
        case Value.RealValue(v) => Some(f(v))
        case _ => None
      }
    )

  private def boolToBoolRule(f: Boolean => Value): Rule =
    Rule(
      BuiltInType.BooleanT, BuiltInType.BooleanT,
      apply = {
        case Value.BooleanValue(v) => Some(f(v))
        case _ => None
      }
    )

  private val rules: Map[(UnOp, BuiltInType), Rule] = Map(
    (UnOp.Plus, BuiltInType.IntegerT) -> intToIntRule(v => Value.IntegerValue(v)),
    (UnOp.Minus, BuiltInType.IntegerT) -> intToIntRule(v => Value.IntegerValue(-v)),
    (UnOp.Plus, BuiltInType.RealT) -> realToRealRule(v => Value.RealValue(v)),
    (UnOp.Minus, BuiltInType.RealT) -> realToRealRule(v => Value.RealValue(-v)),
    (UnOp.Not, BuiltInType.BooleanT) -> boolToBoolRule(v => Value.BooleanValue(!v))
  )

  def inferResultType(innerType: BuiltInType, op: UnOp): Option[BuiltInType] =
    rules
      .get((op, innerType))
      .map(_.resultType)

  def applyOp(op: UnOp, value: Value): Option[Value] =
    rules
      .get((op, value.t))
      .flatMap(_.apply(value))
}