package StdLib.procedures

import SemanticAnalyzer.TypeSymbol
import StdLib.*
import TypeSystem.Value


private def ioProcedureExpected(funcName: String, expected: String, actual: List[Value]) =
  Left(StdLibCallErrMsg(s"Built-in procedure '$funcName' expected $expected, but got: $actual"))

private[StdLib] def StdPrintString = initStdProcedure(
  "printString",
  List(("value", TypeSymbol.StringSym)),
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(text)) =>
        io.write(text)
        Right(())

      case other =>
        ioProcedureExpected("printString", "one string argument", other)
    }
  }
)

private[StdLib] def StdPrintLine = initStdProcedure(
  "printLine",
  List(("value", TypeSymbol.StringSym)),
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(text)) =>
        io.writeLine(text)
        Right(())

      case other =>
        ioProcedureExpected("printLine", "one string argument", other)
    }
  }
)