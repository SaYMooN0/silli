package Interpreter

import Lexer.Loc
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

private def interpretVarDecls(decls: List[VarDeclBoundAstNode]): InterpreterRuntime[Unit] = {
  decls.foldLeft(InterpreterRuntime.pure(())) { (acc, decl) =>
    acc.flatMap { _ =>
      InterpreterRuntime.callstack.flatMap { callstack => fromCallstackResult(callstack.declareVariable(decl.varSym.varName), decl.symLoc)
      }
    }
  }
}

private def interpretProcDecls(decls: List[ProcDeclBoundAstNode]): InterpreterRuntime[Unit] = {
  decls.foldLeft(InterpreterRuntime.pure(())) { (acc, decl) =>
    acc.flatMap { _ =>
      InterpreterRuntime.callstack.flatMap { callstack => fromCallstackResult(callstack.declareProcedureClosure(decl), decl.symLoc)
      }
    }
  }
}
//statements
private def interpretStmt(stmt: StmtBoundAstNode) = stmt match {
  case s: CompoundStmtBoundAstNode => interpretCompoundStmt(s)
  case s: AssignStmtBoundAstNode => interpretAssignStmt(s)
  case s: ProcCallStmtBoundAstNode => interpretProcCallStmt(s)
  case s: IfStmtBoundAstNode => interpretIfStmt(s)
}
private def interpretCompoundStmt(compoundStmt: CompoundStmtBoundAstNode): InterpreterRuntime[Unit] = {
  compoundStmt.stmts.foldLeft(
    InterpreterRuntime.pure(())
  ) {
    case (acc, stmt) => acc.flatMap(_ => interpretStmt(stmt))
  }
}
private def interpretAssignStmt(assignStmt: AssignStmtBoundAstNode): InterpreterRuntime[Unit] = {
  for {
    value <- interpretExpr(assignStmt.typedExpr.expr)
    _ <- InterpreterRuntime.callstack.flatMap { callstack =>
      fromCallstackResult(
        callstack.setVariable(assignStmt.varSym.varName, value),
        assignStmt.loc
      )
    }
  } yield ()
}

private def interpretIfStmt(ifStmt: IfStmtBoundAstNode): InterpreterRuntime[Unit] = {
  for {
    conditionExprVal <- interpretExpr(ifStmt.condition.expr)

    conditionBool <- conditionExprVal match {
      case Value.BooleanValue(booleanVal) => InterpreterRuntime.pure(booleanVal)
      case nonBooleanVal => InterpreterRuntime.failWithInternalErr(ifStmt.conditionLoc, s"If statement condition expression must be boolean, but got: $nonBooleanVal")
    }
    _ <- conditionBool match {
      case true => interpretStmt(ifStmt.thenStmt)
      case false => interpretStmt(ifStmt.elseStmt)
    }
  } yield ()
}

private def interpretProcCallStmt(procCallStmt: ProcCallStmtBoundAstNode): InterpreterRuntime[Unit] = {
  for {
    actualParamValues <- procCallStmt.actualParams.foldLeft(InterpreterRuntime.pure(List.empty[Value])) {
      (acc, typedExpr) =>
        for {
          values <- acc
          value <- interpretExpr(typedExpr.expr)
        } yield values :+ value
    }
    _ <- procCallStmt.procSym.decl match {
      case BuiltinDeclOrigin => InterpreterRuntime.callStdLibProcedure(procCallStmt.procSym, actualParamValues, procCallStmt.loc)
      case _: UserDeclOrigin => interpretUserProcCall(procCallStmt.procSym, actualParamValues, procCallStmt.loc)
    }
  } yield ()
}


private def interpretUserProcCall(procSym: ProcedureSymbol, actualParamValues: List[Value], callLoc: Loc): InterpreterRuntime[Unit] = {
  for {
    procDecl <- InterpreterRuntime.callstack.flatMap { callstack =>
      fromCallstackResult(callstack.enterProcCall(procSym, actualParamValues), callLoc)
    }
    _ <- interpretBlock(procDecl.block)
    _ <- InterpreterRuntime.callstack.flatMap { callstack =>
      InterpreterRuntime.pure(callstack.leaveProcCall())
    }
  } yield ()
}
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
private def interpretVarRefExpr(expr: VarRefBoundAstNode): InterpreterRuntime[Value] =
  InterpreterRuntime.callstack.flatMap { callstack =>
    fromCallstackResult(callstack.getVariable(expr.varSym.varName), expr.loc).flatMap {
      case ValueOrUndefined.Value(value) => InterpreterRuntime.pure(value)
      case ValueOrUndefined.Undefined => InterpreterRuntime.fail(RuntimeErr.UndefinedVariableAccess(expr.loc, expr.varSym.varName))
    }
  }
private def interpretUnOpExpr(expr: UnOpBoundAstNode): InterpreterRuntime[Value] = for {
  innerExpr <- interpretExpr(expr.inner)
  result <- TypeSystem.UnOpRules.applyOp(expr.op, innerExpr) match {
    case Right(res) => InterpreterRuntime.pure(res)
    case Left(err) => failFromOpEvalErr(expr.loc, err)
  }
} yield result

private def interpretBinOpExpr(expr: BinOpBoundAstNode): InterpreterRuntime[Value] = for {
  left <- interpretExpr(expr.left)
  right <- interpretExpr(expr.right)
  result <- TypeSystem.BinOpRules.applyOp(left, expr.op, right) match {
    case Right(res) => InterpreterRuntime.pure(res)
    case Left(err) => failFromOpEvalErr(expr.loc, err)
  }
} yield result

//helpers

private def fromCallstackResult[A](result: Either[CallstackErr, A], loc: Loc): InterpreterRuntime[A] = {
  result match {
    case Right(value) => InterpreterRuntime.pure(value)
    case Left(err) => err match {
      case CallstackErr.VariableNotDeclared(ident) => InterpreterRuntime.fail(RuntimeErr.VariableNotDeclared(loc, ident))
      case CallstackErr.EmptyCallstack => throw new Error(s"Somehow callstack turned out to be empty on the: $loc")
      case CallstackErr.ProcedureClosureNotFound(procId) => throw new Error(s"Procedure closure was not found. Procedure id: $procId. Loc: $loc")
      case CallstackErr.ActualParamsCountMismatch(procName, expected, actual) => throw new Error(s"Procedure '${procName.value}' expected $expected actual params, but got $actual. Loc: $loc")
    }
  }
}
private def failFromOpEvalErr(loc: Loc, err: TypeSystem.OpEvalErr): InterpreterRuntime[Nothing] =
  err match {
    case TypeSystem.OpEvalErr.DivisionByZero => InterpreterRuntime.fail(RuntimeErr.DivisionByZero(loc))
    case TypeSystem.OpEvalErr.IntegerOverflow => InterpreterRuntime.fail(RuntimeErr.IntegerOverflow(loc))
    case TypeSystem.OpEvalErr.RealOverflow => InterpreterRuntime.fail(RuntimeErr.RealOverflow(loc))
    case TypeSystem.OpEvalErr.InvalidRealResult => InterpreterRuntime.fail(RuntimeErr.InvalidRealResult(loc))
    case TypeSystem.OpEvalErr.UnsupportedOperation => InterpreterRuntime.failWithInternalErr(
      loc,
      "Unsupported operation must not pass the semantic analyzer"
    )
  }