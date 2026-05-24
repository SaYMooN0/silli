package TypeSystem.BinOpRules

import TypeSystem.BuiltInType.BooleanT
import TypeSystem.EqualityBinOps.*
import TypeSystem.LogicBinOps.{And, Or, Xor}

private[TypeSystem] object BooleanSupportedBinOps {
  val rules: BinOpRulesMap =
    Map(
      BooleanT -> Map(
        And -> BooleanT,
        Or -> BooleanT,
        Xor -> BooleanT,

        Equal -> BooleanT,
        NotEqual -> BooleanT
      )
    )
}