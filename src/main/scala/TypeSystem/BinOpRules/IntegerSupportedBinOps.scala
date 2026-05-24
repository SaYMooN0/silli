package TypeSystem.BinOpRules

import TypeSystem.*
import TypeSystem.ArithmeticBinOps.*
import TypeSystem.BuiltInType.*
import TypeSystem.ComparisonBinOps.*
import TypeSystem.EqualityBinOps.*

private[TypeSystem] object IntegerSupportedBinOps {
  val rules: BinOpRulesMap = Map(
    RealT -> SharedSupportedBinOps.onNumericWhenOneIsReal,
    IntegerT -> Map(
      Add -> IntegerT,
      Sub -> IntegerT,
      Mul -> IntegerT,
      IntDivBinOp -> IntegerT,
      RealDivBinOp -> RealT,

      Equal -> BooleanT,
      NotEqual -> BooleanT,
      Less -> BooleanT,
      LessOrEqual -> BooleanT,
      Greater -> BooleanT,
      GreaterOrEqual -> BooleanT
    )
  )
}