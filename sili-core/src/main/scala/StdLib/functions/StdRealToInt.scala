package StdLib.functions

import SemanticAnalyzer.TypeSymbol
import StdLib.*
import TypeSystem.Value

private[StdLib] def StdRoundRealToInt = initStdFunction(
  "roundRealToInt",
  List(("value", TypeSymbol.RealSym)),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.RealValue(value)) =>        Right(Value.IntegerValue(math.round(value).toInt))
      case other =>        conversionExpected("roundRealToInt", "one real argument", other)
    }
  }
)

private[StdLib] def StdCeilRealToInt = initStdFunction(
  "ceilRealToInt",
  List(("value", TypeSymbol.RealSym)),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.RealValue(value)) =>        Right(Value.IntegerValue(math.ceil(value).toInt))
      case other =>        conversionExpected("ceilRealToInt", "one real argument", other)
    }
  }
)

private[StdLib] def StdFloorRealToInt = initStdFunction(
  "floorRealToInt",
  List(("value", TypeSymbol.RealSym)),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.RealValue(value)) =>        Right(Value.IntegerValue(math.floor(value).toInt))
      case other =>        conversionExpected("floorRealToInt", "one real argument", other)
    }
  }
)