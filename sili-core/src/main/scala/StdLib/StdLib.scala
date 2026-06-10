package StdLib

import Interpreter.IOCtx
import Lexer.Loc
import Parser.Ident
import SemanticAnalyzer.*
import TypeSystem.Value

object StdLib {
  private val procedureFactories: Seq[ProcedureId => StdProcedure] = List(
    procedures.StdPrintString,
    procedures.StdPrintLine
  )

  private val functionFactories: Seq[FunctionId => StdFunction] = List(
    functions.StdReadLine,
    functions.StdReadSingleCharAsAsciiCode,

    functions.StdStringLength,
    functions.StdStringIndexOf,
    functions.StdStringLastIndexOf,
    functions.StdStringLeftUntilIndex,
    functions.StdStringRightFromIndex,
    functions.StdStringCharAt,
    functions.StdStringSetCharAt,
    functions.StdStringRepeat,
    functions.StdStringFromAsciiCode,
    functions.StdAsciiCodeFromString,
    functions.StdStringConcat,
    functions.StdStringSubstring,

    functions.StdConvertIntegerToString,
    functions.StdConvertRealToString,
    functions.StdConvertBooleanToString,
    functions.StdRoundRealToInt,
    functions.StdCeilRealToInt,
    functions.StdFloorRealToInt
  )

  private val proceduresById: Map[ProcedureId, StdProcedure] =
    procedureFactories
      .zipWithIndex
      .map { case (factory, index) =>
        val id = ProcedureId(-(index + 1))
        id -> factory(id)
      }
      .toMap

  private val functionsById: Map[FunctionId, StdFunction] =
    functionFactories
      .zipWithIndex
      .map { case (factory, index) =>
        val id = FunctionId(-(index + 1))
        id -> factory(id)
      }
      .toMap

  def procedureSymbolsByName: Map[Ident, ProcSymbol] =
    proceduresById
      .values
      .map(stdProc => stdProc.symbol.procName -> stdProc.symbol)
      .toMap

  def functionSymbolsByName: Map[Ident, FuncSymbol] =
    functionsById
      .values
      .map(stdFunc => stdFunc.symbol.funcName -> stdFunc.symbol)
      .toMap

  def tryFindProcedureSymbol(name: Ident): Option[ProcSymbol] =
    procedureSymbolsByName.get(name)

  def tryFindFunctionSymbol(name: Ident): Option[FuncSymbol] =
    functionSymbolsByName.get(name)

  def tryCallProcedure(
                        procId: ProcedureId,
                        actualParamValues: List[Value],
                        io: IOCtx,
                        callLoc: Loc
                      ): Option[Either[StdLibCallErrMsg, Unit]] =
    proceduresById
      .get(procId)
      .map(_.implementation(actualParamValues, io, callLoc))

  def tryCallFunction(
                       funcId: FunctionId,
                       actualParamValues: List[Value],
                       io: IOCtx,
                       callLoc: Loc
                     ): Option[Either[StdLibCallErrMsg, Value]] =
    functionsById
      .get(funcId)
      .map(_.implementation(actualParamValues, io, callLoc))
}

final case class StdLibCallErrMsg(msg: String)


type ProcedureImplementation = (actualParamValues: List[Value], io: IOCtx, callLoc: Loc) => Either[StdLibCallErrMsg, Unit]

final case class StdProcedure(symbol: ProcSymbol, implementation: ProcedureImplementation)

def initStdProcedure(
                      procName: String,
                      paramsNameToType: List[(String, TypeSymbol)],
                      impl: ProcedureImplementation
                    ): (id: ProcedureId) => StdProcedure =
  id => {
    val params = paramsNameToType.map((name, tSym) => ParamSymbol(Ident(name), tSym, BuiltinDeclOrigin));
    val procSym = ProcSymbol(id, Ident(procName), params, 0, BuiltinDeclOrigin);
    StdProcedure(procSym, impl);
  }


type FunctionImplementation = (actualParamValues: List[Value], io: IOCtx, callLoc: Loc) => Either[StdLibCallErrMsg, Value]

final case class StdFunction(symbol: FuncSymbol, implementation: FunctionImplementation)

def initStdFunction(
                     funcName: String,
                     paramsNameToType: List[(String, TypeSymbol)],
                     returnType: TypeSymbol,
                     impl: FunctionImplementation
                   ): (id: FunctionId) => StdFunction =
  id => {
    val params = paramsNameToType.map((name, tSym) => ParamSymbol(Ident(name), tSym, BuiltinDeclOrigin));
    val funcSym = FuncSymbol(id, Ident(funcName), params, returnType, 0, BuiltinDeclOrigin);
    StdFunction(funcSym, impl);
  }