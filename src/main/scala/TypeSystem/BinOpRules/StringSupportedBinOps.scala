package TypeSystem.BinOpRules

import TypeSystem.ArithmeticBinOps.Add
import TypeSystem.BuiltInType.{BooleanT, StringT}
import TypeSystem.EqualityBinOps.{Equal, NotEqual}

private[TypeSystem] object StringSupportedBinOps {
  val rules: BinOpRulesMap =
    Map(
      StringT -> Map(
        Add -> StringT,

        Equal -> BooleanT,
        NotEqual -> BooleanT
      )
    )
}