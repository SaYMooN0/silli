package SemanticAnalyzer

import Lexer.Loc
import Parser.Ident
import TypeSystem.{BinOp, UnOp}

final case class BoundAstRoot(programName: (Ident, Loc), block: BlockBoundAstNode)


final case class BlockBoundAstNode(decls: List[DeclItemBoundAstNode], compoundStmt: StmtBoundAstNode.CompoundStmt)

enum DeclItemBoundAstNode {
  case VarDecl(varSym: VarSymbol, symLoc: Loc)
  case ProcDecl(procSym: ProcSymbol, symLoc: Loc, block: BlockBoundAstNode)
  case FuncDecl(funcSym: FuncSymbol, symLoc: Loc, block: BlockBoundAstNode)
}

enum StmtBoundAstNode {
  case CompoundStmt(stmts: List[StmtBoundAstNode])
  case AssignStmt(valueSymbol: ValueSymbol, typedExpr: AnyTypedExpr, loc: Loc)
  case ProcCall(procSym: ProcSymbol, actualParams: List[AnyTypedExpr], loc: Loc)
  case IfStmt(condition: BooleanTypedExpr, conditionLoc: Loc, thenStmt: StmtBoundAstNode, elseStmt: StmtBoundAstNode)
}


//typed expressions
final case class TypedExpr[
  +E <: ExprBoundAstNode,
  +T <: TypeSymbol
](
   expr: E,
   typeSym: T
 )

type AnyTypedExpr = TypedExpr[ExprBoundAstNode, TypeSymbol]

type BooleanTypedExpr = TypedExpr[ExprBoundAstNode, TypeSymbol.BooleanSym.type]

enum ExprBoundAstNode {
  case BooleanLiteral(value: Boolean)
  case IntegerLiteral(value: Int)
  case RealLiteral(value: Double)
  case StringLiteral(value: String)

  case VarRef(valueSymbol: ValueSymbol, loc: Loc)

  case UnOp(inner: ExprBoundAstNode, op: TypeSystem.UnOp, loc: Loc)

  case BinOp(left: ExprBoundAstNode, op: TypeSystem.BinOp, right: ExprBoundAstNode, loc: Loc)

  case FuncCall(funcSym: FuncSymbol, actualParams: List[AnyTypedExpr], loc: Loc)

}
