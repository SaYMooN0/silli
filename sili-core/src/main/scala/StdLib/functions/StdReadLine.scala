package StdLib.functions

import SemanticAnalyzer.TypeSymbol
import StdLib.*
import TypeSystem.Value

private[StdLib] def StdReadLine = initStdFunction(
  "readLine",
  List(),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List() =>
        Right(Value.StringValue(io.readLine()))

      case other =>
        Left(StdLibCallErrMsg(
          s"Built-in function 'readLine' expected no arguments, but got: $other"
        ))
    }
  }
)