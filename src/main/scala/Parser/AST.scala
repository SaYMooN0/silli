package Parser

import Lexer.IdentRules
import Lexer.Loc

class AST {

}

final case class BooleanLiteral(value: Boolean, loc: Loc)

final case class IntegerLiteral(value: Int, loc: Loc)

final case class RealLiteral(value: Double, loc: Loc)

final case class StringLiteral(value: String, loc: Loc)

type AstExpr =
  BooleanLiteral | IntegerLiteral | RealLiteral | StringLiteral
    | AstUnOp
    | AstBinOp
    | AstVarRef


final case class AstUnOp(expr: AstExpr, op: (TypeSystem.UnOp, Loc), loc: Loc)

final case class AstBinOp(left: AstExpr, right: AstExpr, op: (TypeSystem.BinOp, Loc), loc: Loc)

final case class AstVarRef(ident: Ident, loc: Loc)

final case class Ident(value: String) {
  require(
    IdentRules.isCorrectIdentStarter(value.head)
      && value.tail.forall(IdentRules.canBeInIdentifier)
  )
}

type AstStmt =
  AstCompoundStmt
  | AstAssignStmt
  | AstProcCallStmt
  | AstIfStmt

final case class AstCompoundStmt(stmt: List[AstStmt], loc: Loc)

final case class AstAssignStmt(varRef: AstVarRef, expr: AstExpr, loc: Loc)

final case class AstProcCallStmt(procName: (Ident, Loc), actualParams: List[AstExpr], loc: Loc)

final case class AstIfStmt(condition: AstExpr, thenStmt: AstCompoundStmt, elseStmt: AstCompoundStmt, loc: Loc)
