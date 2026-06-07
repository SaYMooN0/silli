package SemanticAnalyzer.visitor

import Parser.AstStmt
import SemanticAnalyzer.*

private def analyzeStmt(stmt: AstStmt): SemanticAnalyzer[Option[StmtBoundAstNode]] = {
  stmt match {
    case s: AstStmt.CompoundStmt => analyzeCompoundStmt(s).map(Some(_))
    case s: AstStmt.AssignStmt   => analyzeAssignStmt(s)
    case s: AstStmt.ProcCallStmt => analyzeProcCallStmt(s)
    case s: AstStmt.IfStmt       => analyzeIfStmt(s)
  }
}
private[SemanticAnalyzer] def analyzeCompoundStmt(compoundStmt: AstStmt.CompoundStmt): SemanticAnalyzer[StmtBoundAstNode.CompoundStmt] =
  compoundStmt.stmts
    .traverseAnalyzerOpt(analyzeStmt)
    .flatMap(stmts => SemanticAnalyzer.pure(StmtBoundAstNode.CompoundStmt(stmts)))

private def analyzeAssignStmt(
                               assignStmt: AstStmt.AssignStmt
                             ): SemanticAnalyzer[Option[StmtBoundAstNode]] =
  for {
    valueSymOpt <- SemanticAnalyzer.currentScope.flatMap { scope =>
      val ident = assignStmt.varRef.ident
      scope.lookup(ident) match {
        case Some(valueSym: ValueSymbol) => SemanticAnalyzer.pure(Some(valueSym))
        case Some(sym)                   => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedValueSym(sym, assignStmt.varRef.loc))
        case None                        => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredValueSym(ident, assignStmt.varRef.loc))
      }
    }

    exprOpt <- analyzeExpr(assignStmt.expr)

    result <- (valueSymOpt, exprOpt) match {
      case (Some(valueSym), Some(exprWithType)) =>
        if (TypeSystem.AssignRules.canBeAssigned(valueSym.typeSym.spec, exprWithType.typeSym.spec)) {
          SemanticAnalyzer.pure(Some(
            StmtBoundAstNode.AssignStmt(
              valueSymbol = valueSym,
              typedExpr = exprWithType,
              loc = assignStmt.loc
            )
          ))
        } else {
          SemanticAnalyzer.reportErrAndMapNone(
            SemanticErr.CannotAssign(valueSym.typeSym, exprWithType.typeSym, assignStmt.loc)
          )
        }

      case _ => SemanticAnalyzer.pure(None)
    }
  } yield result

private def analyzeIfStmt(ifStmt: AstStmt.IfStmt): SemanticAnalyzer[Option[StmtBoundAstNode]] = {
  def analyzeOptionalStmt(stmtOpt: Option[AstStmt]): SemanticAnalyzer[Option[StmtBoundAstNode]] =
    stmtOpt match {
      case Some(stmt) => analyzeStmt(stmt)
      case None       => SemanticAnalyzer.pure(Some(StmtBoundAstNode.CompoundStmt(List.empty)))
    }

  for {
    exprOpt <- analyzeExpr(ifStmt.condition)
    thenStmtOpt <- analyzeOptionalStmt(ifStmt.thenStmt)
    elseStmtOpt <- analyzeOptionalStmt(ifStmt.elseStmt)

    result <- exprOpt match {
      case None => SemanticAnalyzer.pure(None)

      case Some(TypedExpr(expr, TypeSymbol.BooleanSym)) =>
        (thenStmtOpt, elseStmtOpt) match {

          case (Some(thenStmt), Some(elseStmt)) => SemanticAnalyzer.pure(Some(
            StmtBoundAstNode.IfStmt(TypedExpr(expr, TypeSymbol.BooleanSym), ifStmt.condition.loc,
              thenStmt = thenStmt,
              elseStmt = elseStmt
            )
          ))
          case _                                => SemanticAnalyzer.pure(None)
        }

      case Some(TypedExpr(_, notBooleanTypeSym)) => SemanticAnalyzer.reportErrAndMapNone(
        SemanticErr.IncorrectType(TypeSymbol.BooleanSym, notBooleanTypeSym, ifStmt.condition.loc)
      )
    }
  } yield result
}

private def analyzeProcCallStmt(procCallStmt: AstStmt.ProcCallStmt): SemanticAnalyzer[Option[StmtBoundAstNode]] =
  for {
    procSymOpt <- SemanticAnalyzer.currentScope.flatMap { scope =>
      val (procNameIdent, procNameLoc) = procCallStmt.procName

      scope.lookup(procNameIdent) match {
        case Some(procSym: ProcSymbol) =>
          SemanticAnalyzer.pure(Some(procSym))

        case Some(sym) =>
          SemanticAnalyzer.reportErrAndMapNone(
            SemanticErr.ExpectedProcSym(sym, procNameLoc)
          )

        case None =>
          SemanticAnalyzer.reportErrAndMapNone(
            SemanticErr.UndeclaredProcSym(procNameIdent, procNameLoc)
          )
      }
    }

    analyzedParamOpts <- procCallStmt.actualParams.traverseAnalyzer(analyzeExpr)

    result <- procSymOpt match {
      case Some(procSym) =>
        analyzeActualParams(
          formalParams = procSym.formalParams,
          actualParamAsts = procCallStmt.actualParams,
          analyzedParamOpts = analyzedParamOpts,
          callLoc = procCallStmt.loc,
          incorrectCountErr = actualParamsCount =>
            SemanticErr.ProcIncorrectActualParamsCount(procSym, procCallStmt.loc, actualParamsCount)
        ).map {
          case Some(actualParams) => Some(StmtBoundAstNode.ProcCall(procSym, actualParams, procCallStmt.loc))
          case None               => None
        }

      case None => SemanticAnalyzer.pure(None)
    }
  } yield result