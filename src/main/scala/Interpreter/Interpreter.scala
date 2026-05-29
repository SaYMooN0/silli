package Interpreter

import SemanticAnalyzer.*
import TypeSystem.Value


object InterpretationSuccess;


def interpretBoundAst(boundAst: BoundAstRoot, ioCtx: IOCtx): Either[RuntimeErr, InterpretationSuccess.type] = {

  val initialRuntime = RuntimeCtx.init(boundAst.programName._1, ioCtx)
  interpretAstRoot(boundAst).run(initialRuntime) match {
    case Right((_, _)) => Right(InterpretationSuccess)
    case Left(err) => Left(err)
  }
}
private def interpretAstRoot(astRoot: BoundAstRoot): InterpreterRuntime[Unit] =
  interpretBlock(astRoot.block)

private def interpretBlock(block: BlockBoundAstNode): InterpreterRuntime[Unit] =
  for {
    _ <- interpretVarDecls(block.varDecls)
    _ <- interpretProcDecls(block.procDecls)
    _ <- interpretCompoundStmt(block.compoundStmt)
  } yield ()

private def interpretVarDecls(decls: List[VarDeclBoundAstNode]) = InterpreterRuntime.pure(())
private def interpretProcDecls(decls: List[ProcDeclBoundAstNode]) = InterpreterRuntime.pure(())
//statements
private def interpretStmt(stmt: StmtBoundAstNode) = stmt match {
  case s: CompoundStmtBoundAstNode => interpretCompoundStmt(s)
  case s: AssignStmtBoundAstNode => interpretAssignStmt(s)
  case s: ProcCallStmtBoundAstNode => interpretProcCallStmt(s)
  case s: IfStmtBoundAstNode => interpretIfStmt(s)
}
private def interpretCompoundStmt(compoundStmt: CompoundStmtBoundAstNode) = InterpreterRuntime.pure(())
private def interpretAssignStmt(assignStmt: AssignStmtBoundAstNode) = ???
private def interpretProcCallStmt(procCallStmt: ProcCallStmtBoundAstNode) = ???
private def interpretIfStmt(ifStmt: IfStmtBoundAstNode) = ???
//expressions
private def interpretExpr(expr: ExprBoundAstNode): InterpreterRuntime[Value] = expr match {
  case BooleanLiteralBoundAstNode(v) => InterpreterRuntime.pure(Value.BooleanValue(v))
  case IntegerLiteralBoundAstNode(v) => InterpreterRuntime.pure(Value.IntegerValue(v))
  case RealLiteralBoundAstNode(v) => InterpreterRuntime.pure(Value.RealValue(v))
  case StringLiteralBoundAstNode(v) => InterpreterRuntime.pure(Value.StringValue(v))
  case e: VarRefBoundAstNode => interpretVarRefExpr(e)
  case e: UnOpBoundAstNode => interpretUnOpExpr(e)
  case e: BinOpBoundAstNode => interpretBinOpExpr(e)
}
private def interpretVarRefExpr(expr: VarRefBoundAstNode): InterpreterRuntime[Value] = InterpreterRuntime.callstack.flatMap(
  callstack: Callstack => {
    callstack.
  }
)
private def interpretUnOpExpr(expr: UnOpBoundAstNode): InterpreterRuntime[Value] = for {
  innerExpr <- interpretExpr(expr.inner)
  result <- TypeSystem.UnOpRules.applyOp(expr.op, innerExpr) match {
    case Some(res) => InterpreterRuntime.pure(res)
    case None => throw new Error(s"Unsupported unary operations must not pass the semantic analyzer: $expr")
  }
} yield result
private def interpretBinOpExpr(expr: BinOpBoundAstNode): InterpreterRuntime[Value] = for {
  left <- interpretExpr(expr.left)
  right <- interpretExpr(expr.right)
  result <- TypeSystem.BinOpRules.applyOp(left, expr.op, right) match {
    case Some(res) => InterpreterRuntime.pure(res)
    case None => throw new Error(s"Unsupported binary operations must not pass the semantic analyzer: $expr")
  }
} yield result