package SemanticAnalyzer

import Lexer.Loc
import Parser.Ident
import TypeSystem.BuiltInType


object BuiltinDeclOrigin;

final case class UserDeclOrigin(declLoc: Loc);
type DeclOrigin = BuiltinDeclOrigin.type | UserDeclOrigin

enum TypeSymbol(spec: BuiltInType) {  //only built in types are supported for now
  case IntegerSym extends TypeSymbol(BuiltInType.IntegerT)
  case RealSym extends TypeSymbol(BuiltInType.RealT)
  case BooleanSym extends TypeSymbol(BuiltInType.BooleanT)
  case StringSym extends TypeSymbol(BuiltInType.StringT)
}

final case class VariableSymbol(varName: Ident, varType: TypeSymbol, declOrigin: UserDeclOrigin);

final case class ProcedureSymbol(
                                  id: ProcedureId,
                                  procName: Ident,
                                  formalParams: List[VariableSymbol],
                                  scopeLevel: Int,
                                  decl: DeclOrigin
                                );

opaque type ProcedureId = Int

object ProcedureId {
  def apply(value: Int): ProcedureId = value

  extension (id: ProcedureId)
    def value: Int = id
}
