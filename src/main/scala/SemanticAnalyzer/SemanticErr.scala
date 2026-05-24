package SemanticAnalyzer

import Lexer.Loc
import Parser.Ident


enum SemanticErr(loc: Loc) {
  case SymAlreadyDeclared(newDeclLoc: Loc, existingDecl: SemanticSymbol) extends SemanticErr(newDeclLoc)

  case UndeclaredVarSym(varName: Ident, loc: Loc) extends SemanticErr(loc)
  case ExpectedVarSym(receivedSym: SemanticSymbol, receivedSymLoc: Loc) extends SemanticErr(receivedSymLoc)

  case UndeclaredTypeSym(typeName: Ident, loc: Loc) extends SemanticErr(loc)
  case ExpectedTypeSym(receivedSym: SemanticSymbol, receivedSymLoc: Loc) extends SemanticErr(receivedSymLoc)

  case CannotAssign(target: TypeSymbol, received: TypeSymbol, assignLoc: Loc) extends SemanticErr(assignLoc)
  case InvalidBinOp(binOp: TypeSystem.BinOp, left: TypeSymbol, right: TypeSymbol, binOpLoc: Loc) extends SemanticErr(binOpLoc)
}