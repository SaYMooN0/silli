package Interpreter

import Lexer.{Loc, Pos}
import Parser.Ident
import SemanticAnalyzer.SemanticErr

enum RuntimeErr(val loc: Loc) extends RuntimeException {
  case VariableNotDeclared(override val loc: Loc, ident: Ident) extends RuntimeErr(loc)
  case UndefinedVariableAccess(override val loc: Loc, ident: Ident) extends RuntimeErr(loc)

}