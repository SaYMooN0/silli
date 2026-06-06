package Parser

import Lexer.{IdentRules, Loc}

import scala.compiletime.erasedValue


final case class AstRoot(programName: (Ident, Loc), block: AstBlock)

final case class AstBlock(
                           decls: List[AstDeclarationItem],
                           compoundStmt: AstCompoundStmt,
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
                                   varRefs: List[AstExpr.AstVarRef],
                                   typeAnnotation: (Ident, Loc),
                                   loc: Loc
                                 );

final case class AstFormalParam(
                                 varRef: (Ident, Loc),
                                 typeAnnotation: (Ident, Loc),
                                 loc: Loc
                               )

// expressions
enum AstExpr {
  case BooleanLiteral(value: Boolean, loc: Loc)
  case IntegerLiteral(value: Int, loc: Loc)
  case RealLiteral(value: Double, loc: Loc)
  case StringLiteral(value: String, loc: Loc)
  case UnOp(expr: AstExpr, op: (TypeSystem.UnOp, Loc), loc: Loc)
  case BinOp(left: AstExpr, right: AstExpr, op: (TypeSystem.BinOp, Loc), loc: Loc)
  case FuncCall(funcName: (Ident, Loc), actualParams: List[AstExpr], loc: Loc)

  case AstVarRef(ident: Ident, loc: Loc)

  def loc: Loc = this match {
    case AstExpr.BooleanLiteral(_, loc) => loc
    case AstExpr.IntegerLiteral(_, loc) => loc
    case AstExpr.RealLiteral(_, loc)    => loc
    case AstExpr.StringLiteral(_, loc)  => loc
    case AstExpr.UnOp(_, _, loc)        => loc
    case AstExpr.BinOp(_, _, _, loc)    => loc
    case AstExpr.FuncCall(_, _, loc)    => loc
    case AstExpr.AstVarRef(_, loc)      => loc
  }

  def nodeName: String = this match {
    case _: AstExpr.BooleanLiteral => "boolean literal"
    case _: AstExpr.IntegerLiteral => "integer literal"
    case _: AstExpr.RealLiteral    => "real literal"
    case _: AstExpr.StringLiteral  => "string literal"
    case _: AstExpr.UnOp           => "unary operation expression"
    case _: AstExpr.BinOp          => "binary operation expression"
    case _: AstExpr.FuncCall       => "function call expression"
    case _: AstExpr.AstVarRef      => "variable reference"
  }
}

final case class Ident(value: String) {
  require(
    value.nonEmpty
      && IdentRules.isCorrectIdentStarter(value.head)
      && value.tail.forall(IdentRules.canBeInIdentifier)
  )
}

enum AstStmt {
  case CompoundStmt(stmts: List[AstStmt], loc: Loc)
  case AssignStmt(varRef: AstExpr.AstVarRef, expr: AstExpr, loc: Loc)
  case ProcCallStmt(procName: (Ident, Loc), actualParams: List[AstExpr], loc: Loc)
  case IfStmt(condition: AstExpr, thenStmt: Option[AstStmt], elseStmt: Option[AstStmt], loc: Loc)

  def loc: Loc = this match {
    case AstStmt.CompoundStmt(_, loc)    => loc
    case AstStmt.AssignStmt(_, _, loc)   => loc
    case AstStmt.ProcCallStmt(_, _, loc) => loc
    case AstStmt.IfStmt(_, _, _, loc)    => loc
  }

  def nodeName: String = this match {
    case _: AstStmt.CompoundStmt => "compound statement"
    case _: AstStmt.AssignStmt   => "assignment statement"
    case _: AstStmt.ProcCallStmt => "procedure call statement"
    case _: AstStmt.IfStmt       => "if statement"
  }
}
