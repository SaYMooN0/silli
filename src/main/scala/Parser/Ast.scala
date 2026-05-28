package Parser

import Lexer.{IdentRules, Loc}

import scala.compiletime.erasedValue


final case class AstRoot(programName: (Ident, Loc), block: AstBlock)

final case class AstBlock(
                           varDecls: List[AstVarDeclGroup],
                           procDecls: List[AstProcDecl],
                           compoundStmt: AstCompoundStmt,
                           loc: Loc
                         )

final case class AstVarDeclGroup(
                                  varRefs: List[AstVarRef],
                                  typeAnnotation: (Ident, Loc),
                                  loc: Loc
                                )

final case class AstProcDecl(
                              procName: (Ident, Loc),
                              formalParams: List[AstFormalParam],
                              block: AstBlock,
                              loc: Loc
                            )

final case class AstFormalParam(
                                 varRef: (Ident, Loc),
                                 typeAnnotation: (Ident, Loc),
                                 loc: Loc
                               )

//expressions
sealed trait AstExpr {
  def loc: Loc
}

final case class AstBooleanLiteral(value: Boolean, loc: Loc) extends AstExpr

final case class AstIntegerLiteral(value: Int, loc: Loc) extends AstExpr

final case class AstRealLiteral(value: Double, loc: Loc) extends AstExpr

final case class AstStringLiteral(value: String, loc: Loc) extends AstExpr

final case class AstUnOp(
                          expr: AstExpr,
                          op: (TypeSystem.UnOp, Loc),
                          loc: Loc
                        ) extends AstExpr

final case class AstBinOp(
                           left: AstExpr,
                           right: AstExpr,
                           op: (TypeSystem.BinOp, Loc),
                           loc: Loc
                         ) extends AstExpr

final case class AstVarRef(ident: Ident, loc: Loc) extends AstExpr

final case class Ident(value: String):
  require(
    value.nonEmpty
      && IdentRules.isCorrectIdentStarter(value.head)
      && value.tail.forall(IdentRules.canBeInIdentifier)
  )

//statements
sealed trait AstStmt {
  def loc: Loc
}

final case class AstCompoundStmt(stmts: List[AstStmt], loc: Loc) extends AstStmt

final case class AstAssignStmt(varRef: AstVarRef, expr: AstExpr, loc: Loc) extends AstStmt

final case class AstProcCallStmt(procName: (Ident, Loc), actualParams: List[AstExpr], loc: Loc) extends AstStmt

final case class AstIfStmt(
                            condition: AstExpr,
                            thenStmt: Option[AstStmt],
                            elseStmt: Option[AstStmt],
                            loc: Loc
                          ) extends AstStmt

object AstNodeName {

  inline def of[A <: AstExpr | AstStmt]: String =
    inline erasedValue[A] match {
      case _: AstBooleanLiteral => "boolean literal"
      case _: AstRealLiteral => "real literal"
      case _: AstStringLiteral => "string literal"
      case _: AstUnOp => "unary operation expression"
      case _: AstBinOp => "binary operation expression"
      case _: AstVarRef => "variable reference"
      case _: AstCompoundStmt => "compound statement"
      case _: AstAssignStmt => "assignment statement"
      case _: AstProcCallStmt => "procedure call statement"
      case _: AstIfStmt => "if statement"
      case _: AstExpr => "expression"
      case _: AstStmt => "statement"
    }
}