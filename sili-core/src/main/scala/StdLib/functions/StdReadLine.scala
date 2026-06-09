package StdLib.functions

import SemanticAnalyzer.TypeSymbol
import StdLib.*
import TypeSystem.Value

private[StdLib] def StdRead = initStdFunction(
  "read",
  List(),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List() => Right(Value.StringValue(io.read()))
      case other  => Left(StdLibCallErrMsg(s"Built-in function 'read' expected no arguments, but got: $other"))
    }
  }
)