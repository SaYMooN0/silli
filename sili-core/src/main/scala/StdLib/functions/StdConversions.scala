package StdLib.functions


import SemanticAnalyzer.TypeSymbol
import StdLib.*
import TypeSystem.Value

private def conversionExpected(funcName: String, expected: String, actual: List[Value]) =
  Left(StdLibCallErrMsg(s"Built-in function '$funcName' expected $expected, but got: $actual"))

private[StdLib] def StdConvertIntegerToString = initStdFunction(
  "convertIntegerToString",
  List(("value", TypeSymbol.IntegerSym)),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.IntegerValue(value)) => Right(Value.StringValue(value.toString))
      case other                           => conversionExpected("convertIntegerToString", "one integer argument", other)
    }
  }
)

private[StdLib] def StdConvertRealToString = initStdFunction(
  "convertRealToString",
  List(("value", TypeSymbol.RealSym)),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.RealValue(value)) => Right(Value.StringValue(value.toString))
      case other                        => conversionExpected("convertRealToString", "one real argument", other)
    }
  }
)

private[StdLib] def StdConvertBooleanToString = initStdFunction(
  "convertBooleanToString",
  List(("value", TypeSymbol.BooleanSym)),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.BooleanValue(value)) => Right(Value.StringValue(value.toString))

      case other => conversionExpected("convertBooleanToString", "one boolean argument", other)
    }
  }
)
private[StdLib] def StdConvertBooleanToInteger = initStdFunction(
  "convertBooleanToInteger",
  List(("value", TypeSymbol.BooleanSym)),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.BooleanValue(false)) => Right(Value.IntegerValue(0))
      case List(Value.BooleanValue(true))  => Right(Value.IntegerValue(1))
      case other                           =>
        conversionExpected("convertBooleanToInteger", "one boolean argument", other)
    }
  }
)

private[StdLib] def StdConvertIntegerToBoolean = initStdFunction(
  "convertIntegerToBoolean",
  List(("value", TypeSymbol.IntegerSym)),
  TypeSymbol.BooleanSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.IntegerValue(0)) => Right(Value.BooleanValue(false))
      case List(Value.IntegerValue(_)) => Right(Value.BooleanValue(true))
      case other                       => conversionExpected("convertIntegerToBoolean", "one integer argument", other)
    }
  }
)

private[StdLib] def StdConvertStringToInteger = initStdFunction(
  "convertStringToInteger",
  List(("value", TypeSymbol.StringSym)),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value)) =>
        value.trim.toIntOption match {
          case Some(parsed) => Right(Value.IntegerValue(parsed))
          case None         => Left(StdLibCallErrMsg(s"Cannot convert string to integer: '$value'"))
        }

      case other => conversionExpected("convertStringToInteger", "one string argument", other)
    }
  }
)

private[StdLib] def StdConvertStringToReal = initStdFunction(
  "convertStringToReal",
  List(("value", TypeSymbol.StringSym)),
  TypeSymbol.RealSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value)) =>
        value.trim.toDoubleOption match {
          case Some(parsed) => Right(Value.RealValue(parsed))
          case None         => Left(StdLibCallErrMsg(s"Cannot convert string to real: '$value'"))
        }
      case other                          => conversionExpected("convertStringToReal", "one string argument", other)
    }
  }
)

private[StdLib] def StdCanConvertStringToInteger = initStdFunction(
  "canConvertStringToInteger",
  List(("value", TypeSymbol.StringSym)),
  TypeSymbol.BooleanSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value)) => Right(Value.BooleanValue(value.trim.toIntOption.isDefined))
      case other                          => conversionExpected("canConvertStringToInteger", "one string argument", other)
    }
  }
)

private[StdLib] def StdCanConvertStringToReal = initStdFunction(
  "canConvertStringToReal",
  List(("value", TypeSymbol.StringSym)),
  TypeSymbol.BooleanSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value)) => Right(Value.BooleanValue(value.trim.toDoubleOption.isDefined))
      case other                          => conversionExpected("canConvertStringToReal", "one string argument", other)
    }
  }
)