package StdLib.functions

import SemanticAnalyzer.TypeSymbol
import StdLib.*
import TypeSystem.Value

private def ioFunctionExpected(funcName: String, expected: String, actual: List[Value]) =
  Left(StdLibCallErrMsg(s"Built-in function '$funcName' expected $expected, but got: $actual"))

private[StdLib] def StdReadLine = initStdFunction(
  "readLine",
  List.empty,
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List() =>
        Right(Value.StringValue(io.readLine()))

      case other =>
        ioFunctionExpected("readLine", "no arguments", other)
    }
  }
)

private[StdLib] def StdReadSingleCharAsAsciiCode = initStdFunction(
  "readSingleCharAsAsciiCode",
  List.empty,
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List() =>
        Right(Value.IntegerValue(io.readSingleCharAsAsciiCode()))

      case other =>
        ioFunctionExpected("readSingleCharAsAsciiCode", "no arguments", other)
    }
  }
) 