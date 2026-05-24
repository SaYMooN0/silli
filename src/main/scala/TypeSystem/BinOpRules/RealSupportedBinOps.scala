package TypeSystem.BinOpRules

import TypeSystem.*
import TypeSystem.ArithmeticBinOps.*
import TypeSystem.BuiltInType.*
import TypeSystem.ComparisonBinOps.*
import TypeSystem.EqualityBinOps.*

private[TypeSystem] object RealSupportedBinOps {

  val rules: BinOpRulesMap = Map(
    IntegerT -> SharedSupportedBinOps.onNumericWhenOneIsReal,
    RealT -> SharedSupportedBinOps.onNumericWhenOneIsReal
  )
}