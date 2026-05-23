package SemanticAnalyzer

import Lexer.Loc
import Parser.Ident


enum SemanticErr(loc: Loc) {
  case UndefinedVarSym(varIdent: (Ident, Loc)) extends SemanticErr(varIdent._2)
}