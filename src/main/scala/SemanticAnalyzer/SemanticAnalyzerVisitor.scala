package SemanticAnalyzer

import Lexer.Loc
import Parser.*

def analyzeProgramAst(ast: AstRoot): Either[List[SemanticErr], BoundAstRoot] = {
  val (boundAstRoot, finalCtx) = analyzeAstRoot(ast).run(SemanticCtx.init)
  val errors = finalCtx.errors.toList

  if (errors.nonEmpty) Left(errors)
  else Right(boundAstRoot)
}
private def analyzeAstRoot(ast: AstRoot): SemanticAnalyzer[BoundAstRoot] =
  for {
    block <- analyzeBlock(ast.block)
  } yield BoundAstRoot(ast.programName, block)

private def analyzeBlock(block: AstBlock): SemanticAnalyzer[BlockBoundAstNode] =
  for {
    varDecls <- analyzeVarDecls(block.varDecls)
    procDecls <- analyzeProcDecls(block.procDecls)
    compoundStmt <- analyzeCompoundStmt(block.compoundStmt)
  } yield BlockBoundAstNode(varDecls, procDecls, compoundStmt)

private def analyzeVarDecls(groups: List[AstVarDeclGroup]): SemanticAnalyzer[List[VarDeclBoundAstNode]] =
  groups.traverseAnalyzer(analyzeVarDeclGroup).map(_.flatten)

private def analyzeVarDeclGroup(group: AstVarDeclGroup): SemanticAnalyzer[List[VarDeclBoundAstNode]] = {
  for {
    typeSymOpt <- analyzeVarDeclGroupTypeAnnotation(group.typeAnnotation)
    decls <- group.varRefs.traverseAnalyzerOpt { varRef =>
      analyzeVarDeclInGroup(varRef, typeSymOpt)
    }
  } yield decls
}

private def analyzeVarDeclGroupTypeAnnotation(typeAnnotation: (Ident, Loc)): SemanticAnalyzer[Option[TypeSymbol]] = {
  SemanticAnalyzer.currentScope.flatMap { scope =>
    val typeIdent = typeAnnotation._1
    val typeLoc = typeAnnotation._2
    scope.lookup(typeIdent) match {
      case Some(typeSym: TypeSymbol) => SemanticAnalyzer.pure(Some(typeSym))
      case Some(received) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedTypeSym(received, typeLoc))
      case None => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredTypeSym(typeIdent, typeLoc))
    }
  }
}

private def analyzeVarDeclInGroup(
                                   varRef: AstVarRef,
                                   typeSymOpt: Option[TypeSymbol]
                                 ): SemanticAnalyzer[Option[VarDeclBoundAstNode]] = {
  SemanticAnalyzer.currentScope.flatMap { scope =>
    val varIdent = varRef.ident
    (scope.lookupLocal(varIdent), typeSymOpt) match {
      case (Some(alreadyDeclared), _) =>
        SemanticAnalyzer.reportErrAndMapNone(SemanticErr.SymAlreadyDeclared(varRef.loc, alreadyDeclared))
      case (None, Some(typeSym)) => {
        val varSym = VariableSymbol(varRef.ident, typeSym, UserDeclOrigin(varRef.loc))
        SemanticAnalyzer
          .addSymbolToCurrentScope(varIdent, varSym)
          .map(_ => Some(VarDeclBoundAstNode(varSym)))
      }
      case (None, _) => SemanticAnalyzer.pure(None)
    }
  }
}
private def analyzeProcDecls(decls: List[AstProcDecl]): SemanticAnalyzer[List[ProcDeclBoundAstNode]] =
  SemanticAnalyzer.pure(List.empty)

//statements
private def analyzeStmt(stmt: AstStmt): SemanticAnalyzer[Option[StmtBoundAstNode]] = {
  stmt match {
    case s: AstCompoundStmt => analyzeCompoundStmt(s).map(Some(_))
    case s: AstAssignStmt => analyzeAssignStmt(s)
    case s: AstProcCallStmt => analyzeProcCallStmt(s)
    case s: AstIfStmt => analyzeIfStmt(s)
  }
}
private def analyzeCompoundStmt(compoundStmt: AstCompoundStmt): SemanticAnalyzer[CompoundStmtBoundAstNode] = {
  for {
    checkedStmts <- compoundStmt.stmts.traverseAnalyzerOpt(analyzeStmt)
  } yield CompoundStmtBoundAstNode(checkedStmts)
}
private def analyzeAssignStmt(assignStmt: AstAssignStmt): SemanticAnalyzer[Option[AssignStmtBoundAstNode]] = {
  for {
    varSymOpt <- SemanticAnalyzer.currentScope.flatMap { scope =>
      val varIdent = assignStmt.varRef.ident
      scope.lookup(varIdent) match {
        case Some(varSym: VariableSymbol) => SemanticAnalyzer.pure(Some(varSym))
        case Some(sym) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedVarSym(sym, assignStmt.varRef.loc))
        case None => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredVarSym(varIdent, assignStmt.varRef.loc))
      }
    }

    exprOpt <- analyzeExpr(assignStmt.expr)

    result <- (varSymOpt, exprOpt) match {
      case (Some(varSym), Some(expr)) =>
        if (TypeSystem.AssignRules.canBeAssigned(varSym.typeSym.spec, expr.typeSym.spec)) {
          SemanticAnalyzer.pure(Some(AssignStmtBoundAstNode(varSym, expr)))
        } else {
          SemanticAnalyzer.reportErrAndMapNone(SemanticErr.CannotAssign(varSym.typeSym, expr.typeSym, assignStmt.loc))
        }
      case _ => SemanticAnalyzer.pure(None)
    }
  } yield result
}
private def analyzeIfStmt(ifStmt: AstIfStmt): SemanticAnalyzer[Option[IfStmtBoundAstNode]] = {
  def analyzeOptionalStmt(stmtOpt: Option[AstStmt]): SemanticAnalyzer[Option[StmtBoundAstNode]] =
    stmtOpt match {
      case Some(stmt) => analyzeStmt(stmt)
      case None => SemanticAnalyzer.pure(Some(CompoundStmtBoundAstNode(List.empty)))
    }

  for {
    exprOpt <- analyzeExpr(ifStmt.condition)
    thenStmtOpt <- analyzeOptionalStmt(ifStmt.thenStmt)
    elseStmtOpt <- analyzeOptionalStmt(ifStmt.elseStmt)

    result <- exprOpt match {
      case None => SemanticAnalyzer.pure(None)
      case Some(TypedExpr(expr, TypeSymbol.BooleanSym)) =>
        (thenStmtOpt, elseStmtOpt) match {
          case (Some(thenStmt), Some(elseStmt)) => SemanticAnalyzer.pure(Some(IfStmtBoundAstNode(TypedExpr(expr, TypeSymbol.BooleanSym), thenStmt, elseStmt)))
          case _ => SemanticAnalyzer.pure(None)
        }
      case Some(TypedExpr(_, notBooleanTypeSym)) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.IncorrectType(TypeSymbol.BooleanSym, notBooleanTypeSym, ifStmt.condition.loc))
    }
  } yield result
}
private def analyzeProcCallStmt(procCallStmt: AstProcCallStmt): SemanticAnalyzer[Option[ProcCallStmtBoundAstNode]] = {
  for {
    procSymOpt <- SemanticAnalyzer.currentScope.flatMap { scope =>
      val (procNameIdent, procNameLoc) = procCallStmt.procName
      scope.lookup(procNameIdent) match {
        case Some(procSym: ProcedureSymbol) => SemanticAnalyzer.pure(Some(procSym))
        case Some(sym) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedProcSym(sym, procNameLoc))
        case None => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredVarSym(procNameIdent, procNameLoc))
      }
    }
    analyzedParamOpts <- procCallStmt.actualParams.traverseAnalyzer(analyzeExpr)
    result <- procSymOpt match {
      case Some(procSym) => analyzeProcCallWithResolvedProc(procCallStmt, procSym, analyzedParamOpts)
      case None => SemanticAnalyzer.pure(None)
    }
  } yield result
}

private def analyzeProcCallWithResolvedProc(
                                             procCallStmt: AstProcCallStmt,
                                             procSym: ProcedureSymbol,
                                             analyzedParamOpts: List[Option[AnyTypedExpr]]
                                           ): SemanticAnalyzer[Option[ProcCallStmtBoundAstNode]] = {
  val actualParamsCount = procCallStmt.actualParams.length
  if (procSym.formalParams.length != actualParamsCount) {
    SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ProcIncorrectActualParamsCount(procSym, procCallStmt.loc, actualParamsCount))
  } else {
    for {
      paramTypeChecks <- procSym.formalParams
        .zip(procCallStmt.actualParams)
        .zip(analyzedParamOpts)
        .traverseAnalyzer({
          case ((_, _), None) => SemanticAnalyzer.pure(false) // all errors are handled in analyzeExpr
          case ((formalParam, actualParamAst), Some(actualParamExpr)) =>
            if (TypeSystem.AssignRules.canBeAssigned(formalParam.typeSym.spec, actualParamExpr.typeSym.spec)) {
              SemanticAnalyzer.pure(true)
            } else {
              SemanticAnalyzer
                .reportErr(SemanticErr.CannotAssign(formalParam.typeSym, actualParamExpr.typeSym, actualParamAst.loc))
                .map(_ => false)
            }
        })
      result = {
        val allExprsWereAnalyzed = analyzedParamOpts.forall(_.isDefined)
        val allTypesAreCorrect = paramTypeChecks.forall(identity)
        if (allExprsWereAnalyzed && allTypesAreCorrect) {
          Some(ProcCallStmtBoundAstNode(procSym, analyzedParamOpts.flatten))
        } else None
      }
    } yield result
  }
}
//expressions
private def analyzeExpr(expr: AstExpr): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  expr match {
    case l: AstBooleanLiteral => SemanticAnalyzer.pure(Some(TypedExpr(BooleanLiteralBoundAstNode(l.value), TypeSymbol.BooleanSym)))
    case l: AstRealLiteral => SemanticAnalyzer.pure(Some(TypedExpr(RealLiteralBoundAstNode(l.value), TypeSymbol.RealSym)))
    case l: AstIntegerLiteral => SemanticAnalyzer.pure(Some(TypedExpr(IntegerLiteralBoundAstNode(l.value), TypeSymbol.IntegerSym)))
    case l: AstStringLiteral => SemanticAnalyzer.pure(Some(TypedExpr(StringLiteralBoundAstNode(l.value), TypeSymbol.StringSym)))
    case varRef: AstVarRef => analyzeVarRef(varRef)
    case unOp: AstUnOp => analyzeUnOp(unOp)
    case binOp: AstBinOp => analyzeBinOp(binOp)
  }
}
private def analyzeVarRef(varRef: AstVarRef): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  val varIdent = varRef.ident
  SemanticAnalyzer.currentScope.flatMap {
    scope =>
      scope.lookup(varIdent) match {
        case Some(varSym: VariableSymbol) => SemanticAnalyzer.pure(Some(TypedExpr(
          VarRefBoundAstNode(varSym), varSym.typeSym
        )))
        case Some(sym) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedVarSym(sym, varRef.loc))
        case None => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredVarSym(varIdent, varRef.loc))
      }

  }
}
private def analyzeUnOp(node: AstUnOp): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  for {
    innerExpr <- analyzeExpr(node.expr)
    result <- innerExpr match {
      case Some(TypedExpr(innerExpr, exprType)) =>
        TypeSystem.UnOpRules.inferResultType(exprType.spec, node.op._1) match {
          case None => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.InvalidUnOp(
            node.op._1, exprType, node.op._2
          ))
          case Some(resultType) => SemanticAnalyzer.pure(Some(TypedExpr(
            UnOpBoundAstNode(innerExpr, node.op._1), TypeSymbol.fromType(resultType)
          )))
        }
      case _ => SemanticAnalyzer.pure(None)
    }
  } yield result
}
private def analyzeBinOp(node: AstBinOp): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  for {
    leftExpr <- analyzeExpr(node.left)
    rightExpr <- analyzeExpr(node.right)
    result <- (leftExpr, rightExpr) match {
      case (Some(TypedExpr(lExpr, lType)), Some(TypedExpr(rExpr, rType))) =>
        TypeSystem.BinOpRules.inferResultType(lType.spec, node.op._1, rType.spec) match {
          case None => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.InvalidBinOp(
            node.op._1, lType, rType, node.op._2
          ))
          case Some(resultType) => SemanticAnalyzer.pure(Some(TypedExpr(
            BinOpBoundAstNode(lExpr, node.op._1, rExpr),
            TypeSymbol.fromType(resultType)
          )))
        }
      case _ => SemanticAnalyzer.pure(None)
    }
  } yield result
}

//helpers
extension [A](items: List[A]) {
  private def traverseAnalyzer[B](f: A => SemanticAnalyzer[B]): SemanticAnalyzer[List[B]] =
    items.foldRight(SemanticAnalyzer.pure(List.empty[B])) { (item, acc) =>
      for {
        head <- f(item)
        tail <- acc
      } yield head :: tail
    }

  def traverseAnalyzerOpt[B](f: A => SemanticAnalyzer[Option[B]]): SemanticAnalyzer[List[B]] =
    items.traverseAnalyzer(f).map(_.flatten)
}