package SemanticAnalyzer

import Lexer.Loc
import Parser.Ident
import TypeSystem.BuiltInType


type SemanticSymbol = TypeSymbol | VariableSymbol | ProcedureSymbol;

enum TypeSymbol(val spec: BuiltInType) { //only built in types are supported for now
  case IntegerSym extends TypeSymbol(BuiltInType.IntegerT)
  case RealSym extends TypeSymbol(BuiltInType.RealT)
  case BooleanSym extends TypeSymbol(BuiltInType.BooleanT)
  case StringSym extends TypeSymbol(BuiltInType.StringT)
}

object TypeSymbol {
  def fromType(t: BuiltInType): TypeSymbol = t match {
    case BuiltInType.IntegerT => TypeSymbol.IntegerSym
    case BuiltInType.RealT => TypeSymbol.RealSym
    case BuiltInType.BooleanT => TypeSymbol.BooleanSym
    case BuiltInType.StringT => TypeSymbol.StringSym
  }
}

final case class VariableSymbol(varName: Ident, typeSym: TypeSymbol, declOrigin: UserDeclOrigin);

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


object BuiltinDeclOrigin;

final case class UserDeclOrigin(declLoc: Loc);
type DeclOrigin = BuiltinDeclOrigin.type | UserDeclOrigin
