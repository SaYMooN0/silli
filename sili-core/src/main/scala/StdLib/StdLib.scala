package StdLib

import Interpreter.IOCtx
import Lexer.{Loc, Pos}
import Parser.Ident
import SemanticAnalyzer.{BuiltinDeclOrigin, DeclOrigin, ProcedureId, ProcSymbol, SemanticSymbol, TypeSymbol, UserDeclOrigin, VarSymbol}
import TypeSystem.Value

object StdLib {
  private val procedureFactories: List[ProcedureId => StdProcedure] =
    List(
      procedures.StdPrint
    )

  private val proceduresById: Map[ProcedureId, StdProcedure] =
    procedureFactories
      .zipWithIndex
      .map { case (factory, index) =>
        val id = ProcedureId(-(index + 1))
        id -> factory(id)
      }
      .toMap

  def procedureSymbolsByName: Map[Ident, ProcSymbol] =
    proceduresById
      .values
      .map(stdProc => stdProc.symbol.procName -> stdProc.symbol)
      .toMap

  def tryFindProcedureSymbol(name: Ident): Option[ProcSymbol] =
    procedureSymbolsByName.get(name)

  def tryCallProcedure(
                        procId: ProcedureId,
                        actualParamValues: List[Value],
                        io: IOCtx,
                        callLoc: Loc
                      ): Option[Either[StdLibCallErrMsg, Unit]] =
    proceduresById
      .get(procId)
      .map(_.implementation(actualParamValues, io, callLoc))
}


final case class StdLibCallErrMsg(msg: String);
private def StdParamDeclOrigin: UserDeclOrigin = UserDeclOrigin(Loc(Pos(1, 1), Pos(1, 2)))

type ProcedureImplementation = (actualParamValues: List[Value], io: IOCtx, callLoc: Loc) => Either[StdLibCallErrMsg, Unit];

final case class StdProcedure(symbol: ProcSymbol, implementation: ProcedureImplementation);

def initStdProcedure(
                      procedureName: String,
                      paramsNameToType: List[(String, TypeSymbol)],
                      implementation: ProcedureImplementation
                    ): (id: ProcedureId) => StdProcedure = id => {
  val params = paramsNameToType.map((name, tSym) => VarSymbol(Ident(name), tSym, StdParamDeclOrigin)).toList;
  val procSym = ProcSymbol(id, Ident(procedureName), params, 0, BuiltinDeclOrigin);
  StdProcedure(procSym, implementation);
}