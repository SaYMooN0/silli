package Parser

import Lexer.{IdentRules, Loc}

import scala.compiletime.erasedValue


final case class AstRoot(programName: (Ident, Loc), block: AstBlock)

final case class AstBlock(
                           decls: List[AstDeclarationItem],
                           compoundStmt: AstStmt.CompoundStmt,
                           loc: Loc
                         )

enum AstDeclarationItem {
  case AstVarGroupDecl(varDecls: List[AstTypedVarsDecl])
  case AstProcDecl(
                    procName: (Ident, Loc),
                    formalParams: List[AstFormalParam],
                    block: AstBlock,
                    loc: Loc
                  )

  case AstFuncDecl(
                    funcName: (Ident, Loc),
                    formalParams: List[AstFormalParam],
                    block: AstBlock,
                    typeAnnotation: (Ident, Loc),
                    loc: Loc
                  )
}

final case class AstTypedVarsDecl(
                                   varRefs: List[AstExpr.VarRef],
                                   typeAnnotation: (Ident, Loc),
                                   loc: Loc
                                 )

final case class AstFormalParam(
                                 varRef: (Ident, Loc),
                                 typeAnnotation: (Ident, Loc),
                                 loc: Loc
                               )

enum AstExpr(val loc: Loc) {
  case BooleanLiteral(value: Boolean, override val loc: Loc) extends AstExpr(loc)
  case IntegerLiteral(value: Int, override val loc: Loc) extends AstExpr(loc)
  case RealLiteral(value: Double, override val loc: Loc) extends AstExpr(loc)
  case StringLiteral(value: String, override val loc: Loc) extends AstExpr(loc)
  case UnOp(expr: AstExpr, op: (TypeSystem.UnOp, Loc), override val loc: Loc) extends AstExpr(loc)
  case BinOp(left: AstExpr, right: AstExpr, op: (TypeSystem.BinOp, Loc), override val loc: Loc) extends AstExpr(loc)
  case FuncCall(funcName: (Ident, Loc), actualParams: List[AstExpr], override val loc: Loc) extends AstExpr(loc)
  case VarRef(ident: Ident, override val loc: Loc) extends AstExpr(loc)

}

enum AstStmt(val loc: Loc) {
  case CompoundStmt(stmts: List[AstStmt], override val loc: Loc) extends AstStmt(loc)
  case AssignStmt(varRef: AstExpr.VarRef, expr: AstExpr, override val loc: Loc) extends AstStmt(loc)
  case ProcCallStmt(procName: (Ident, Loc), actualParams: List[AstExpr], override val loc: Loc) extends AstStmt(loc)
  case IfStmt(condition: AstExpr, thenStmt: Option[AstStmt], elseStmt: Option[AstStmt], override val loc: Loc) extends AstStmt(loc)

}

final case class Ident(value: String) {
  require(
    value.nonEmpty
      && IdentRules.isCorrectIdentStarter(value.head)
      && value.tail.forall(IdentRules.canBeInIdentifier)
  )
}