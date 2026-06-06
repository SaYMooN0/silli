package SemanticAnalyzer

import Lexer.Loc
import Parser.Ident


enum SemanticErr(loc: Loc) {

  case SymAlreadyDeclared(newDeclLoc: Loc, existingDecl: SemanticSymbol) extends SemanticErr(newDeclLoc)

  case UndeclaredProcSym(procName: Ident, loc: Loc) extends SemanticErr(loc)
  case ExpectedProcSym(receivedSym: SemanticSymbol, receivedSymLoc: Loc) extends SemanticErr(receivedSymLoc)
  case ProcIncorrectActualParamsCount(sym: ProcSymbol, callLoc: Loc, actualParamsCount: Int) extends SemanticErr(callLoc)


  case UndeclaredVarSym(varName: Ident, loc: Loc) extends SemanticErr(loc)
  case ExpectedVarSym(receivedSym: SemanticSymbol, receivedSymLoc: Loc) extends SemanticErr(receivedSymLoc)

  case UndeclaredTypeSym(typeName: Ident, loc: Loc) extends SemanticErr(loc)
  case ExpectedTypeSym(receivedSym: SemanticSymbol, receivedSymLoc: Loc) extends SemanticErr(receivedSymLoc)

  case CannotAssign(target: TypeSymbol, received: TypeSymbol, assignLoc: Loc) extends SemanticErr(assignLoc)
  case InvalidBinOp(binOp: TypeSystem.BinOp, left: TypeSymbol, right: TypeSymbol, binOpLoc: Loc) extends SemanticErr(binOpLoc)
  case InvalidUnOp(unOp: TypeSystem.UnOp, innerNodeType: TypeSymbol, unOpLoc: Loc) extends SemanticErr(unOpLoc)

  case IncorrectType(receivedType: TypeSymbol, expected: TypeSymbol, receivedTypeLoc: Loc) extends SemanticErr(receivedTypeLoc)

  case FuncResultNotAssigned(funcSym: FuncSymbol, loc: Loc) extends SemanticErr(loc)
  case CannotPassAsParam(target: TypeSymbol, received: TypeSymbol, actualParamLoc: Loc) extends SemanticErr(actualParamLoc)
  case ResultSymbolUsedAsParam(paramLoc: Loc) extends SemanticErr(paramLoc)
}
