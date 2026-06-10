package SemanticAnalyzer.visitor

import Parser.AstExpr
import SemanticAnalyzer.*

private def analyzeExpr(expr: AstExpr): SemanticAnalyzer[Option[AnyTypedExpr]] =
  expr match {
    case AstExpr.BooleanLiteral(value, _) => SemanticAnalyzer.pure(Some(TypedExpr(
      ExprBoundAstNode.BooleanLiteral(value), TypeSymbol.BooleanSym
    )))

    case AstExpr.IntegerLiteral(value, _) => SemanticAnalyzer.pure(Some(TypedExpr(
      ExprBoundAstNode.IntegerLiteral(value),
      TypeSymbol.IntegerSym
    )))

    case AstExpr.RealLiteral(value, _) => SemanticAnalyzer.pure(Some(TypedExpr(
      ExprBoundAstNode.RealLiteral(value),
      TypeSymbol.RealSym
    )))

    case AstExpr.StringLiteral(value, _) => SemanticAnalyzer.pure(Some(TypedExpr(
      ExprBoundAstNode.StringLiteral(value),
      TypeSymbol.StringSym
    )))

    case varRef: AstExpr.VarRef     => analyzeVarRef(varRef)
    case unOp: AstExpr.UnOp         => analyzeUnOp(unOp)
    case binOp: AstExpr.BinOp       => analyzeBinOp(binOp)
    case funcCall: AstExpr.FuncCall => analyzeFuncCallExpr(funcCall)
  }

private def analyzeVarRef(varRef: AstExpr.VarRef): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  val ident = varRef.ident

  SemanticAnalyzer.currentScope.flatMap { scope =>
    scope.lookup(ident) match {
      case Some(vSym: ValueSymbol) => SemanticAnalyzer.pure(Some(TypedExpr(
        ExprBoundAstNode.VarRef(vSym, varRef.loc),
        vSym.typeSym
      )))

      case Some(sym) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedValueSym(sym, varRef.loc))
      case None      => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredValueSym(ident, varRef.loc))

    }
  }
}

private def analyzeUnOp(node: AstExpr.UnOp): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  for {
    innerExprOpt <- analyzeExpr(node.expr)
    result <- innerExprOpt match {
      case None                                 => SemanticAnalyzer.pure(None)
      case Some(TypedExpr(innerExpr, exprType)) =>
        TypeSystem.UnOpRules.inferResultType(exprType.spec, node.op._1) match {
          case None             => SemanticAnalyzer.reportErrAndMapNone(
            SemanticErr.InvalidUnOp(node.op._1, exprType, node.op._2)
          )
          case Some(resultType) => SemanticAnalyzer.pure(Some(TypedExpr(
            ExprBoundAstNode.UnOp(innerExpr, node.op._1, node.loc),
            TypeSymbol.fromType(resultType)
          )))
        }

    }
  } yield result
}
private def analyzeBinOp(node: AstExpr.BinOp): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  for {
    leftExprOpt <- analyzeExpr(node.left)
    rightExprOpt <- analyzeExpr(node.right)

    result <- (leftExprOpt, rightExprOpt) match {
      case (Some(TypedExpr(leftExpr, leftType)), Some(TypedExpr(rightExpr, rightType))) => {
        TypeSystem.BinOpRules.inferResultType(leftType.spec, node.op._1, rightType.spec) match {
          case None             => SemanticAnalyzer.reportErrAndMapNone(
            SemanticErr.InvalidBinOp(node.op._1, leftType, rightType, node.op._2)
          )
          case Some(resultType) => SemanticAnalyzer.pure(Some(TypedExpr(
            ExprBoundAstNode.BinOp(leftExpr, node.op._1, rightExpr, node.loc),
            TypeSymbol.fromType(resultType)
          )))
        }
      }

      case _ => SemanticAnalyzer.pure(None)
    }
  } yield result
}
private def analyzeFuncCallExpr(funcCall: AstExpr.FuncCall): SemanticAnalyzer[Option[AnyTypedExpr]] =
  for {
    funcSymOpt <- SemanticAnalyzer.currentScope.flatMap { scope =>
      val (funcNameIdent, funcNameLoc) = funcCall.funcName

      scope.lookup(funcNameIdent) match {
        case Some(s: FuncSymbol) => SemanticAnalyzer.pure(Some(s))
        case Some(sym)           => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedFuncSym(sym, funcNameLoc))
        case None                => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredFuncSym(funcNameIdent, funcNameLoc))
      }
    }

    analyzedParamOpts <- funcCall.actualParams.traverseAnalyzer(analyzeExpr)

    result <- funcSymOpt match {
      case Some(funcSym) =>
        analyzeActualParams(
          formalParams = funcSym.formalParams,
          actualParamAsts = funcCall.actualParams,
          analyzedParamOpts = analyzedParamOpts,
          callLoc = funcCall.loc,
          incorrectCountErr = actualParamsCount =>
            SemanticErr.FuncIncorrectActualParamsCount(funcSym, funcCall.loc, actualParamsCount)
        ).map {
          case Some(actualParams) =>
            Some(
              TypedExpr(
                ExprBoundAstNode.FuncCall(
                  funcSym = funcSym,
                  actualParams = actualParams,
                  loc = funcCall.loc
                ),
                funcSym.returnType
              )
            )

          case None =>
            None
        }

      case None =>
        SemanticAnalyzer.pure(None)
    }
  } yield result