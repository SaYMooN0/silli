package SemanticAnalyzer

import Lexer.Loc
import Parser.Ident
import TypeSystem.BuiltInType


type SemanticSymbol = TypeSymbol | ValueSymbol | ProcSymbol | FuncSymbol

enum TypeSymbol(val spec: BuiltInType) { //only built in types are supported for now
  case IntegerSym extends TypeSymbol(BuiltInType.IntegerT)
  case RealSym extends TypeSymbol(BuiltInType.RealT)
  case BooleanSym extends TypeSymbol(BuiltInType.BooleanT)
  case StringSym extends TypeSymbol(BuiltInType.StringT)
}

object TypeSymbol {
  def fromType(t: BuiltInType): TypeSymbol = t match {
    case BuiltInType.IntegerT => TypeSymbol.IntegerSym
    case BuiltInType.RealT    => TypeSymbol.RealSym
    case BuiltInType.BooleanT => TypeSymbol.BooleanSym
    case BuiltInType.StringT  => TypeSymbol.StringSym
  }
}

sealed trait ValueSymbol {
  def name: Ident

  def typeSym: TypeSymbol

  def declOrigin: DeclOrigin
}

final case class VarSymbol(name: Ident, typeSym: TypeSymbol, declOrigin: UserDeclOrigin) extends ValueSymbol

final case class ParamSymbol(name: Ident, typeSym: TypeSymbol, declOrigin: DeclOrigin) extends ValueSymbol

final case class FuncResultSymbol(typeSym: TypeSymbol, declOrigin: UserDeclOrigin) extends ValueSymbol {
  override val name: Ident = FuncResultSymbol.ResultIdent
}

object FuncResultSymbol {
  def ResultIdent: Ident = Ident("result")
}

final case class ProcSymbol(
                             id: ProcedureId,
                             procName: Ident,
                             formalParams: List[ParamSymbol],
                             scopeLevel: Int,
                             decl: DeclOrigin
                           )

final case class FuncSymbol(
                             id: FunctionId,
                             funcName: Ident,
                             formalParams: List[ParamSymbol],
                             returnType: TypeSymbol,
                             scopeLevel: Int,
                             decl: DeclOrigin
                           )

type DeclOrigin = BuiltinDeclOrigin.type | UserDeclOrigin

object BuiltinDeclOrigin

final case class UserDeclOrigin(declLoc: Loc)

opaque type ProcedureId = Int

object ProcedureId {
  def apply(value: Int): ProcedureId = value

  extension (id: ProcedureId)
    def value: Int = id
}

opaque type FunctionId = Int

object FunctionId {
  def apply(value: Int): FunctionId = value

  extension (id: FunctionId)
    def value: Int = id
}

