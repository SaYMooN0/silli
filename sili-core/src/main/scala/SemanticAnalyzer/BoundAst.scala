package SemanticAnalyzer

import Lexer.Loc
import Parser.Ident
import TypeSystem.{BinOp, UnOp}

final case class BoundAstRoot(programName: (Ident, Loc), block: BlockBoundAstNode)


final case class BlockBoundAstNode(
                                    varDecls: List[VarDeclBoundAstNode],
                                    procDecls: List[ProcDeclBoundAstNode],
                                    compoundStmt: CompoundStmtBoundAstNode
                                  )

final case class VarDeclBoundAstNode(varSym: VariableSymbol, symLoc: Loc);

final case class ProcDeclBoundAstNode(procSym: ProcedureSymbol, symLoc: Loc, block: BlockBoundAstNode);


//statements

sealed trait StmtBoundAstNode;

final case class CompoundStmtBoundAstNode(stmts: List[StmtBoundAstNode]) extends StmtBoundAstNode

final case class AssignStmtBoundAstNode(varSym: VariableSymbol, typedExpr: AnyTypedExpr) extends StmtBoundAstNode

final case class ProcCallStmtBoundAstNode(procSym: ProcedureSymbol, actualParams: List[AnyTypedExpr], loc: Loc) extends StmtBoundAstNode

final case class IfStmtBoundAstNode(
                                     condition: BooleanTypedExpr,
                                     conditionLoc: Loc,
                                     thenStmt: StmtBoundAstNode,
                                     elseStmt: StmtBoundAstNode
                                   ) extends StmtBoundAstNode

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

//expressions
sealed trait ExprBoundAstNode;

final case class BooleanLiteralBoundAstNode(value: Boolean) extends ExprBoundAstNode

final case class IntegerLiteralBoundAstNode(value: Int) extends ExprBoundAstNode

final case class RealLiteralBoundAstNode(value: Double) extends ExprBoundAstNode

final case class StringLiteralBoundAstNode(value: String) extends ExprBoundAstNode

final case class VarRefBoundAstNode(varSym: VariableSymbol, loc: Loc) extends ExprBoundAstNode

final case class UnOpBoundAstNode(inner: ExprBoundAstNode, op: UnOp) extends ExprBoundAstNode

final case class BinOpBoundAstNode(left: ExprBoundAstNode, op: BinOp, right: ExprBoundAstNode) extends ExprBoundAstNode
