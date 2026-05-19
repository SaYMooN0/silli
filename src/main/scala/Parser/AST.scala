package Parser

import Lexer.IdentRules
import Lexer.Loc

import scala.compiletime.erasedValue


final case class AstRoot(programName: (Ident, Loc), block: AstBlock)

final case class AstBlock(
                           varDecls: List[AstVarDecl],
                           procDecls: List[AstProcDecl],
                           compoundStmt: AstCompoundStmt,
                           loc: Loc
                         )

final case class AstVarDecl(
                             varRef: AstVarRef,
                             typeAnnotation: (TypeSystem.TypeSpec, Loc),
                             loc: Loc
                           )

final case class AstProcDecl(
                              procName: (Ident, Loc),
                              formalParams: List[AstFormalParam],
                              bloc: AstBlock,
                              loc: Loc
                            )

final case class AstFormalParam(
                                 varRef: AstVarRef,
                                 typeAnnotation: (TypeSystem.TypeSpec, Loc),
                                 loc: Loc
                               )

type BasicASTNode = AstExpr | AstStmt

final case class BooleanLiteral(value: Boolean, loc: Loc)

final case class IntegerLiteral(value: Int, loc: Loc)

final case class RealLiteral(value: Double, loc: Loc)

final case class StringLiteral(value: String, loc: Loc)

final case class AstUnOp(
                          expr: AstExpr,
                          op: (TypeSystem.UnOp, Loc),
                          loc: Loc
                        )

final case class AstBinOp(
                           left: AstExpr,
                           right: AstExpr,
                           op: (TypeSystem.BinOp, Loc),
                           loc: Loc
                         )

final case class AstVarRef(ident: Ident, loc: Loc)

final case class Ident(value: String) {
  require(
    IdentRules.isCorrectIdentStarter(value.head)
      && value.tail.forall(IdentRules.canBeInIdentifier)
  )
}

type AstExpr =
  BooleanLiteral
    | IntegerLiteral
    | RealLiteral
    | StringLiteral
    | AstUnOp
    | AstBinOp
    | AstVarRef

final case class AstCompoundStmt(stmt: List[AstStmt], loc: Loc)

final case class AstAssignStmt(varRef: AstVarRef, expr: AstExpr, loc: Loc)

final case class AstProcCallStmt(
                                  procName: (Ident, Loc),
                                  actualParams: List[AstExpr],
                                  loc: Loc
                                )

final case class AstIfStmt(
                            condition: AstExpr,
                            thenStmt: AstCompoundStmt,
                            elseStmt: AstCompoundStmt,
                            loc: Loc
                          )

type AstStmt =
  AstCompoundStmt
    | AstAssignStmt
    | AstProcCallStmt
    | AstIfStmt

object AstNodeName {

  inline def of[A <: BasicASTNode]: String =
    inline erasedValue[A] match {
      case _: BooleanLiteral => "boolean literal"
      case _: RealLiteral => "real literal"
      case _: StringLiteral => "string literal"
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