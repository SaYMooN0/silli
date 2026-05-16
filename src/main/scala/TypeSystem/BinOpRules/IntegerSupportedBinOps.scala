package TypeSystem.BinOpRules

import TypeSystem.*
import TypeSystem.ArithmeticBinOps.*
import TypeSystem.ComparisonBinOps.*
import TypeSystem.EqualityBinOps.*
import TypeSystem.TypeSpec.*

object IntegerSupportedBinOps {
  val rules: List[BinOpRule] =
    List(
      BinOpRule(Add, IntegerT, IntegerT),
      BinOpRule(Sub, IntegerT, IntegerT),
      BinOpRule(Mul, IntegerT, IntegerT),

      BinOpRule(Add, RealT, RealT),
      BinOpRule(Sub, RealT, RealT),
      BinOpRule(Mul, RealT, RealT),

      BinOpRule(IntDivBinOp, IntegerT, IntegerT),
      BinOpRule(RealDivBinOp, IntegerT, RealT),
      BinOpRule(RealDivBinOp, RealT, RealT),

      BinOpRule(Equal, IntegerT, BooleanT),
      BinOpRule(NotEqual, IntegerT, BooleanT),

      BinOpRule(Equal, RealT, BooleanT),
      BinOpRule(NotEqual, RealT, BooleanT),

      BinOpRule(Less, IntegerT, BooleanT),
      BinOpRule(LessOrEqual, IntegerT, BooleanT),
      BinOpRule(Greater, IntegerT, BooleanT),
      BinOpRule(GreaterOrEqual, IntegerT, BooleanT),

      BinOpRule(Less, RealT, BooleanT),
      BinOpRule(LessOrEqual, RealT, BooleanT),
      BinOpRule(Greater, RealT, BooleanT),
      BinOpRule(GreaterOrEqual, RealT, BooleanT)
    )

}