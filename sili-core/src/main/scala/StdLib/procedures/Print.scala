package StdLib.procedures

import Interpreter.IOCtx
import Lexer.Loc
import Parser.Ident
import SemanticAnalyzer.{BuiltinDeclOrigin, ProcedureId, ProcSymbol, TypeSymbol, VarSymbol}
import StdLib.*
import TypeSystem.Value


private[StdLib] def StdPrint = initStdProcedure(
  "print",
  List(("value", TypeSymbol.StringSym)),
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(text)) => {
        io.writeLine(text)
        Right(())
      }

      case other => Left(StdLibCallErrMsg(s"Built-in procedure 'print' expected one string argument, but got: $other"))
    }
  }
)