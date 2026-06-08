package Interpreter

import Lexer.Loc
import SemanticAnalyzer.*
import TypeSystem.Value


object InterpretationSuccess


def interpretBoundAst(boundAst: BoundAstRoot, ioCtx: IOCtx): Either[RuntimeErr, InterpretationSuccess.type] = {
  val initialRuntime = RuntimeCtx.init(boundAst.programName._1, ioCtx)
  interpretAstRoot(boundAst).run(initialRuntime) match {
    case Right((_, _)) => Right(InterpretationSuccess)
    case Left(err)     => Left(err)
  }
}
private def interpretAstRoot(astRoot: BoundAstRoot): InterpreterRuntime[Unit] =
  interpretBlock(astRoot.block)

private def interpretBlock(block: BlockBoundAstNode): InterpreterRuntime[Unit] =
  for {
    _ <- interpretBlockDecls(block.decls)
    _ <- interpretCompoundStmt(block.compoundStmt)
  } yield ()
private def interpretBlockDecls(decls: List[DeclItemBoundAstNode]): InterpreterRuntime[Unit] = {
  decls.foldLeft(InterpreterRuntime.pure(())) { (acc, decl) =>
    acc.flatMap { _ =>
      InterpreterRuntime.callstack.flatMap { callstack =>
        decl match {
          case DeclItemBoundAstNode.VarDecl(sym, loc) => fromCallstackResult(callstack.declareVariable(sym.name), loc)
          case procD: DeclItemBoundAstNode.ProcDecl   => fromCallstackResult(callstack.declareProcedureClosure(procD), procD.symLoc)
          case funcD: DeclItemBoundAstNode.FuncDecl   => fromCallstackResult(callstack.declareFunctionClosure(funcD), funcD.symLoc)
        }
      }
    }
  }
}
//statements
private def interpretStmt(stmt: StmtBoundAstNode) = stmt match {
  case s: StmtBoundAstNode.CompoundStmt => interpretCompoundStmt(s)
  case s: StmtBoundAstNode.AssignStmt   => interpretAssignStmt(s)
  case s: StmtBoundAstNode.ProcCall     => interpretProcCallStmt(s)
  case s: StmtBoundAstNode.IfStmt       => interpretIfStmt(s)
}
private def interpretCompoundStmt(compoundStmt: StmtBoundAstNode.CompoundStmt): InterpreterRuntime[Unit] = {
  compoundStmt.stmts.foldLeft(
    InterpreterRuntime.pure(())
  ) {
    case (acc, stmt) => acc.flatMap(_ => interpretStmt(stmt))
  }
}
private def interpretAssignStmt(assignStmt: StmtBoundAstNode.AssignStmt): InterpreterRuntime[Unit] = {
  for {
    value <- interpretExpr(assignStmt.typedExpr.expr)
    _ <- InterpreterRuntime.callstack.flatMap { callstack =>
      fromCallstackResult(
        callstack.setVariable(assignStmt.valueSymbol.name, value),
        assignStmt.loc
      )
    }
  } yield ()
}

private def interpretIfStmt(ifStmt: StmtBoundAstNode.IfStmt): InterpreterRuntime[Unit] = {
  for {
    conditionExprVal <- interpretExpr(ifStmt.condition.expr)

    conditionBool <- conditionExprVal match {
      case Value.BooleanValue(booleanVal) => InterpreterRuntime.pure(booleanVal)
      case nonBooleanVal                  => InterpreterRuntime.failWithInternalErr(ifStmt.conditionLoc, s"If statement condition expression must be boolean, but got: $nonBooleanVal")
    }
    _ <- if (conditionBool) {
      interpretStmt(ifStmt.thenStmt)
    } else {
      interpretStmt(ifStmt.elseStmt)
    }
  } yield ()
}

private def interpretProcCallStmt(procCallStmt: StmtBoundAstNode.ProcCall): InterpreterRuntime[Unit] = {
  for {
    actualParamValues <- interpretActualParamValues(procCallStmt.actualParams)
    _ <- procCallStmt.procSym.decl match {
      case BuiltinDeclOrigin => InterpreterRuntime.callStdLibProcedure(procCallStmt.procSym, actualParamValues, procCallStmt.loc)
      case UserDeclOrigin(_) => interpretUserProcCall(procCallStmt.procSym, actualParamValues, procCallStmt.loc)
    }
  } yield ()
}

private def interpretUserProcCall(
                                   procSym: ProcSymbol,
                                   actualParamValues: List[Value],
                                   callLoc: Loc
                                 ): InterpreterRuntime[Unit] = {
  withEnteredUserCall(
    enter = _.enterProcCall(procSym, actualParamValues),
    callLoc = callLoc
  ) { procDecl => interpretBlock(procDecl.block) }
}
//expressions
private def interpretExpr(expr: ExprBoundAstNode): InterpreterRuntime[Value] = expr match {
  case ExprBoundAstNode.BooleanLiteral(v) => InterpreterRuntime.pure(Value.BooleanValue(v))
  case ExprBoundAstNode.IntegerLiteral(v) => InterpreterRuntime.pure(Value.IntegerValue(v))
  case ExprBoundAstNode.RealLiteral(v)    => InterpreterRuntime.pure(Value.RealValue(v))
  case ExprBoundAstNode.StringLiteral(v)  => InterpreterRuntime.pure(Value.StringValue(v))
  case e: ExprBoundAstNode.VarRef         => interpretVarRefExpr(e)
  case e: ExprBoundAstNode.FuncCall       => interpretFuncCallExpr(e)
  case e: ExprBoundAstNode.UnOp           => interpretUnOpExpr(e)
  case e: ExprBoundAstNode.BinOp          => interpretBinOpExpr(e)
}
private def interpretVarRefExpr(expr: ExprBoundAstNode.VarRef): InterpreterRuntime[Value] = {
  InterpreterRuntime.callstack.flatMap { callstack =>
    fromCallstackResult(callstack.getVariable(expr.valueSymbol.name), expr.loc).flatMap {
      case ValueOrUndefined.Value(value) => InterpreterRuntime.pure(value)
      case ValueOrUndefined.Undefined    => InterpreterRuntime.fail(RuntimeErr.UndefinedVariableAccess(expr.loc, expr.valueSymbol.name))
    }
  }
}
private def interpretFuncCallExpr(funcCallExpr: ExprBoundAstNode.FuncCall): InterpreterRuntime[Value] = {
  for {
    actualParamValues <- interpretActualParamValues(funcCallExpr.actualParams)
    result <- funcCallExpr.funcSym.decl match {
      case BuiltinDeclOrigin => InterpreterRuntime.callStdLibFunction(funcCallExpr.funcSym, actualParamValues, funcCallExpr.loc)
      case UserDeclOrigin(_) => interpretUserFuncCall(funcCallExpr.funcSym, actualParamValues, funcCallExpr.loc)
    }
  } yield result
}
private def interpretUserFuncCall(
                                   funcSym: FuncSymbol,
                                   actualParamValues: List[Value],
                                   callLoc: Loc
                                 ): InterpreterRuntime[Value] = {
  withEnteredUserCall(
    enter = _.enterFuncCall(funcSym, actualParamValues),
    callLoc = callLoc
  ) { funcDecl =>
    for {
      _ <- interpretBlock(funcDecl.block)
      result <- InterpreterRuntime.callstack.flatMap { callstack =>
        fromCallstackResult(
          callstack.getVariable(FuncResultSymbol.ResultIdent),
          callLoc
        ).flatMap {
          case ValueOrUndefined.Value(value) =>
            InterpreterRuntime.pure(value)

          case ValueOrUndefined.Undefined =>
            InterpreterRuntime.fail(
              RuntimeErr.UndefinedVariableAccess(callLoc, FuncResultSymbol.ResultIdent)
            )
        }
      }
    } yield result
  }
}
private def interpretUnOpExpr(expr: ExprBoundAstNode.UnOp): InterpreterRuntime[Value] = for {
  innerExpr <- interpretExpr(expr.inner)
  result <- TypeSystem.UnOpRules.applyOp(expr.op, innerExpr) match {
    case Right(res) => InterpreterRuntime.pure(res)
    case Left(err)  => failFromOpEvalErr(expr.loc, err)
  }
} yield result

private def interpretBinOpExpr(expr: ExprBoundAstNode.BinOp): InterpreterRuntime[Value] = for {
  left <- interpretExpr(expr.left)
  right <- interpretExpr(expr.right)
  result <- TypeSystem.BinOpRules.applyOp(left, expr.op, right) match {
    case Right(res) => InterpreterRuntime.pure(res)
    case Left(err)  => failFromOpEvalErr(expr.loc, err)
  }
} yield result

//helpers
private def withEnteredUserCall[D, A](
                                       enter: Callstack => Either[CallstackErr, D],
                                       callLoc: Loc
                                     )
                                     (body: D => InterpreterRuntime[A]): InterpreterRuntime[A] = {
  for {
    decl <- InterpreterRuntime.callstack.flatMap { callstack =>
      fromCallstackResult(enter(callstack), callLoc)
    }

    result <- body(decl)

    _ <- InterpreterRuntime.callstack.flatMap { callstack =>
      InterpreterRuntime.pure(callstack.leaveCall())
    }
  } yield result
}
private def interpretActualParamValues(actualParams: List[AnyTypedExpr]): InterpreterRuntime[List[Value]] = {
  actualParams.foldLeft(InterpreterRuntime.pure(List.empty[Value])) {
    (acc, typedExpr) =>
      for {
        values <- acc
        value <- interpretExpr(typedExpr.expr)
      } yield values :+ value
  }
}

private def fromCallstackResult[A](result: Either[CallstackErr, A], loc: Loc): InterpreterRuntime[A] = {
  result match {
    case Right(value) => InterpreterRuntime.pure(value)
    case Left(err)    => err match {
      case CallstackErr.VariableNotDeclared(ident)                            => InterpreterRuntime.fail(RuntimeErr.VariableNotDeclared(loc, ident))
      case CallstackErr.EmptyCallstack                                        => throw new Error(s"Somehow callstack turned out to be empty on the: $loc")
      case CallstackErr.ActualParamsCountMismatch(procName, expected, actual) => throw new Error(s"Procedure '${procName.value}' expected $expected actual params, but got $actual. Loc: $loc")
      case CallstackErr.ProcedureClosureNotFound(procId)                      => throw new Error(s"Procedure closure was not found. Procedure id: $procId. Loc: $loc")
      case CallstackErr.FunctionClosureNotFound(funcId)                            => throw new Error(s"Function closure was not found. Function id: $funcId. Loc: $loc")
    }
  }
}
private def failFromOpEvalErr(loc: Loc, err: TypeSystem.OpEvalErr): InterpreterRuntime[Nothing] =
  err match {
    case TypeSystem.OpEvalErr.DivisionByZero       => InterpreterRuntime.fail(RuntimeErr.DivisionByZero(loc))
    case TypeSystem.OpEvalErr.IntegerOverflow      => InterpreterRuntime.fail(RuntimeErr.IntegerOverflow(loc))
    case TypeSystem.OpEvalErr.RealOverflow         => InterpreterRuntime.fail(RuntimeErr.RealOverflow(loc))
    case TypeSystem.OpEvalErr.InvalidRealResult    => InterpreterRuntime.fail(RuntimeErr.InvalidRealResult(loc))
    case TypeSystem.OpEvalErr.UnsupportedOperation => InterpreterRuntime.failWithInternalErr(
      loc,
      "Unsupported operation must not pass the semantic analyzer"
    )
  }
  