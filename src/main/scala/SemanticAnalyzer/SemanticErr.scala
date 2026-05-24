package SemanticAnalyzer

import Lexer.Loc


enum SemanticErr(loc: Loc) {
  case SymAlreadyDeclared(newDeclLoc: Loc, existingDecl: SemanticSymbol) extends SemanticErr(newDeclLoc)

  case UndefinedVarSym(varName: String, loc: Loc) extends SemanticErr(loc)
  case ExpectedVarSym(receivedSym: SemanticSymbol, receivedSymLoc: Loc) extends SemanticErr(receivedSymLoc)

  case UndefinedTypeSym(typeName: String, loc: Loc) extends SemanticErr(loc)
  case ExpectedTypeSym(receivedSym: SemanticSymbol, receivedSymLoc: Loc) extends SemanticErr(receivedSymLoc)

  case CannotAssign(goal: TypeSymbol, received: TypeSymbol, assignLoc: Loc) extends SemanticErr(assignLoc)
}