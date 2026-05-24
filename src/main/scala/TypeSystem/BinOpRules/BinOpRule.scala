package TypeSystem.BinOpRules

import TypeSystem.*
import TypeSystem.ArithmeticBinOps.*
import TypeSystem.BuiltInType.*
import TypeSystem.ComparisonBinOps.*
import TypeSystem.EqualityBinOps.*

final case class BinOpRule(
                            op: BinOp,
                            rightType: BuiltInType,
                            returnType: BuiltInType
                          )

private[TypeSystem] type BinOpRulesMap =
  Map[BuiltInType, Map[BinOp, BuiltInType]]

private[BinOpRules] object SharedSupportedBinOps {
  private[BinOpRules] val onNumericWhenOneIsReal: Map[BinOp, BuiltInType] = Map(
    Add -> RealT,
    Sub -> RealT,
    Mul -> RealT,
    RealDivBinOp -> RealT,

    Equal -> BooleanT,
    NotEqual -> BooleanT,
    Less -> BooleanT,
    LessOrEqual -> BooleanT,
    Greater -> BooleanT,
    GreaterOrEqual -> BooleanT
  );
}