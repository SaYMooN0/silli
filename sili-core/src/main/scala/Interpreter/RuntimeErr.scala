package Interpreter

import Lexer.{Loc, Pos}
import Parser.Ident
import SemanticAnalyzer.SemanticErr

enum RuntimeErr(loc: Loc) {
  case VariableNotDeclared(loc: Loc, ident: Ident) extends RuntimeErr(loc)
  case UndefinedVariableAccess(loc: Loc, ident: Ident) extends RuntimeErr(loc)
  case InternalInterpreterErr(loc: Loc, message: String) extends RuntimeErr(loc)
  case DivisionByZero(loc: Loc) extends RuntimeErr(loc)
  case IntegerOverflow(loc: Loc) extends RuntimeErr(loc)
  case RealOverflow(loc: Loc) extends RuntimeErr(loc)
  case InvalidRealResult(loc: Loc) extends RuntimeErr(loc)
}