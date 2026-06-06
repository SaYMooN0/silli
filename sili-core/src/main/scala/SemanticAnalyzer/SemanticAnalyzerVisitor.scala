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

private def analyzeBlock(block: AstBlock): SemanticAnalyzer[BlockBoundAstNode] = {
  for {
    decls <- analyzeBlockDecls(block.decls)
    compoundStmt <- analyzeCompoundStmt(block.compoundStmt)
  } yield BlockBoundAstNode(decls, compoundStmt)
}


private def analyzeBlockDecls(
                               declItems: List[AstDeclarationItem]
                             ): SemanticAnalyzer[List[DeclItemBoundAstNode]] =
  declItems
    .traverseAnalyzer(analyzeDeclarationItem)
    .map(_.flatten)

private def analyzeDeclarationItem(declItem: AstDeclarationItem): SemanticAnalyzer[List[DeclItemBoundAstNode]] = {
  declItem match {
    case AstDeclarationItem.AstVarGroupDecl(varDecls) => analyzeVarGroupDecl(varDecls)
    case procDecl: AstDeclarationItem.AstProcDecl     => analyzeProcDecl(procDecl).map(_.toList)
    case funcDecl: AstDeclarationItem.AstFuncDecl     => analyzeFuncDecl(funcDecl).map(_.toList)
  }
}


private def analyzeVarGroupDecl(varDecls: List[AstTypedVarsDecl]): SemanticAnalyzer[List[DeclItemBoundAstNode]] = {
  varDecls
    .traverseAnalyzer(analyzeTypedVarsDecl)
    .map(_.flatten)
}

private def analyzeTypedVarsDecl(decl: AstTypedVarsDecl): SemanticAnalyzer[List[DeclItemBoundAstNode]] = {
  for {
    typeSymOpt <- analyzeTypeSpec(decl.typeAnnotation)
    decls <- decl.varRefs.traverseAnalyzerOpt(analyzeVarDeclInGroup(_, typeSymOpt))
  } yield decls
}

private def analyzeVarDeclInGroup(
                                   varRef: AstVarRef,
                                   typeSymOpt: Option[TypeSymbol]
                                 ): SemanticAnalyzer[Option[DeclItemBoundAstNode]] =
  SemanticAnalyzer.currentScope.flatMap { scope =>
    val varIdent = varRef.ident

    (scope.lookupLocal(varIdent), typeSymOpt) match {
      case (Some(alreadyDeclared), _) => SemanticAnalyzer.reportErrAndMapNone(
        SemanticErr.SymAlreadyDeclared(varRef.loc, alreadyDeclared)
      )
      case (None, Some(typeSym))      => {
        val varSym = VarSymbol(varIdent, typeSym, UserDeclOrigin(varRef.loc))

        SemanticAnalyzer
          .addSymbolToCurrentScope(varIdent, varSym)
          .map(_ => Some(DeclItemBoundAstNode.VarDecl(varSym, varRef.loc)))
      }
      case (None, None)               => SemanticAnalyzer.pure(None)
    }
  }

private def analyzeTypeSpec(typeAnnotation: (Ident, Loc)): SemanticAnalyzer[Option[TypeSymbol]] = {
  SemanticAnalyzer.currentScope.flatMap { scope =>
    val typeIdent = typeAnnotation._1
    val typeLoc = typeAnnotation._2
    scope.lookup(typeIdent) match {
      case Some(typeSym: TypeSymbol) => SemanticAnalyzer.pure(Some(typeSym))
      case Some(received)            => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedTypeSym(received, typeLoc))
      case None                      => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredTypeSym(typeIdent, typeLoc))
    }
  }
}

private def analyzeProcDecl(
                             procDecl: AstDeclarationItem.AstProcDecl
                           ): SemanticAnalyzer[Option[DeclItemBoundAstNode.ProcDecl]] = {
  SemanticAnalyzer.currentScope.flatMap { parentScope =>
    val (procIdent, procIdentLoc) = procDecl.procName

    parentScope.lookupLocal(procIdent) match {
      case Some(alreadyDeclared) => SemanticAnalyzer.reportErrAndMapNone(
        SemanticErr.SymAlreadyDeclared(procIdentLoc, alreadyDeclared)
      )

      case None => for {
        formalParamOpts <- SemanticAnalyzer.withScope(parentScope.createChildScopeFromCurrent(procIdent.value)) {
          analyzeRoutineFormalParams(procDecl.formalParams)
        }

        formalParams = formalParamOpts.flatten
        procId <- SemanticAnalyzer.nextProcedureId
        procSym = ProcSymbol(procId, procIdent, formalParams, parentScope.level, UserDeclOrigin(procIdentLoc))

        _ <- SemanticAnalyzer.addSymbolToCurrentScope(procIdent, procSym)
        parentScopeWithProc <- SemanticAnalyzer.currentScope
        procScopeWithParams = formalParams.foldLeft(
          parentScopeWithProc.createChildScopeFromCurrent(procIdent.value)
        ) { (scope, paramSym) => scope.withSymbol(paramSym.name, paramSym) }
        block <- SemanticAnalyzer.withScope(procScopeWithParams) {
          analyzeBlock(procDecl.block)
        }
      } yield Some(DeclItemBoundAstNode.ProcDecl(procSym, procIdentLoc, block))
    }
  }
}

private def analyzeFuncDecl(
                             funcDecl: AstDeclarationItem.AstFuncDecl
                           ): SemanticAnalyzer[Option[DeclItemBoundAstNode.FuncDecl]] = {
  SemanticAnalyzer.currentScope.flatMap { parentScope =>
    val (funcIdent, funcIdentLoc) = funcDecl.funcName

    parentScope.lookupLocal(funcIdent) match {
      case Some(alreadyDeclared) => SemanticAnalyzer.reportErrAndMapNone(
        SemanticErr.SymAlreadyDeclared(funcIdentLoc, alreadyDeclared)
      )

      case None =>
        for {
          returnTypeOpt <- analyzeTypeSpec(funcDecl.typeAnnotation)

          formalParamOpts <- SemanticAnalyzer.withScope(parentScope.createChildScopeFromCurrent(funcIdent.value)) {
            analyzeRoutineFormalParams(funcDecl.formalParams)
          }

          result <- returnTypeOpt match {
            case None             => SemanticAnalyzer.pure(None)
            case Some(returnType) => analyzeFuncDeclWithResolvedReturnType(
              funcDecl, funcIdent, funcIdentLoc, parentScope, formalParamOpts.flatten, returnType
            )
          }
        } yield result
    }
  }
}

private def analyzeFuncDeclWithResolvedReturnType(
                                                   funcDecl: AstDeclarationItem.AstFuncDecl,
                                                   funcIdent: Ident,
                                                   funcIdentLoc: Loc,
                                                   parentScope: GlobalScopeSymbolTable | ScopedSymbolTable,
                                                   formalParams: List[ParamSymbol],
                                                   returnType: TypeSymbol
                                                 ): SemanticAnalyzer[Option[DeclItemBoundAstNode.FuncDecl]] = {
  val resultIdent = Ident("result")
  val resultSym = FuncResultSymbol(returnType, UserDeclOrigin(funcIdentLoc))
  val resultNameAlreadyUsedByParam = formalParams.find(_.name == resultIdent)

  resultNameAlreadyUsedByParam match {
    case Some(paramSym) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ResultSymbolUsedAsParam(
      paramSym.declOrigin match {
        case UserDeclOrigin(loc) => loc
        case BuiltinDeclOrigin   => funcIdentLoc
      }
    ))

    case None => {
      for {
        funcId <- SemanticAnalyzer.nextFunctionId

        funcSym = FuncSymbol(funcId, funcIdent, formalParams, returnType, parentScope.level, UserDeclOrigin(funcIdentLoc))
        _ <- SemanticAnalyzer.addSymbolToCurrentScope(funcIdent, funcSym)
        parentScopeWithFunc <- SemanticAnalyzer.currentScope

        funcScopeWithParams = formalParams.foldLeft(
          parentScopeWithFunc.createChildScopeFromCurrent(funcIdent.value)
        ) { (scope, paramSym) => scope.withSymbol(paramSym.name, paramSym) }

        funcScopeWithResult = funcScopeWithParams.withSymbol(resultSym.name, resultSym)

        block <- SemanticAnalyzer.withScope(funcScopeWithResult) {
          analyzeBlock(funcDecl.block)
        }

        _ <- if (funcResultWasAssigned(block)) {
          SemanticAnalyzer.pure(())
        } else {
          SemanticAnalyzer.reportErr(SemanticErr.FuncResultNotAssigned(funcSym, funcIdentLoc))
        }
      } yield Some(DeclItemBoundAstNode.FuncDecl(funcSym, funcIdentLoc, block))
    }
  }
}
private def funcResultWasAssigned(block: BlockBoundAstNode): Boolean =
  block.compoundStmt.stmts.exists(stmtAssignsFuncResult)

private def stmtAssignsFuncResult(stmt: StmtBoundAstNode): Boolean =
  stmt match {
    case AssignStmtBoundAstNode(_: FuncResultSymbol, _, _) => true
    case CompoundStmtBoundAstNode(stmts)                   => stmts.exists(stmtAssignsFuncResult)
    case IfStmtBoundAstNode(_, _, thenStmt, elseStmt)      =>
      stmtAssignsFuncResult(thenStmt) || stmtAssignsFuncResult(elseStmt)
    case _                                                 => false
  }
private def analyzeRoutineFormalParams(params: List[AstFormalParam]): SemanticAnalyzer[List[Option[ParamSymbol]]] = {
  params.foldLeft(SemanticAnalyzer.pure(List.empty[Option[ParamSymbol]])) {
    case (acc, param) => for {
      gathered <- acc
      analyzed <- analyzeRoutineFormalParam(param)
    } yield gathered :+ analyzed
  }
}
private def analyzeRoutineFormalParam(param: AstFormalParam): SemanticAnalyzer[Option[ParamSymbol]] = {
  SemanticAnalyzer.currentScope.flatMap { scope =>
    val (paramIdent, paramIdentLoc) = param.varRef
    analyzeTypeSpec(param.typeAnnotation).flatMap { typeSymOpt =>
      (scope.lookupLocal(paramIdent), typeSymOpt) match {
        case (None, None)               => SemanticAnalyzer.pure(None)
        case (Some(alreadyDeclared), _) => SemanticAnalyzer.reportErrAndMapNone(
          SemanticErr.SymAlreadyDeclared(paramIdentLoc, alreadyDeclared)
        )
        case (None, Some(typeSym))      => {
          val paramSym = ParamSymbol(paramIdent, typeSym, UserDeclOrigin(paramIdentLoc))
          SemanticAnalyzer
            .addSymbolToCurrentScope(paramIdent, paramSym)
            .map(_ => Some(paramSym))
        }
      }
    }
  }
}
//statements
private def analyzeStmt(stmt: AstStmt): SemanticAnalyzer[Option[StmtBoundAstNode]] = {
  stmt match {
    case s: AstCompoundStmt => analyzeCompoundStmt(s).map(Some(_))
    case s: AstAssignStmt   => analyzeAssignStmt(s)
    case s: AstProcCallStmt => analyzeProcCallStmt(s)
    case s: AstIfStmt       => analyzeIfStmt(s)
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
        case Some(varSym: VarSymbol) => SemanticAnalyzer.pure(Some(varSym))
        case Some(sym)               => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedVarSym(sym, assignStmt.varRef.loc))
        case None                    => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredVarSym(varIdent, assignStmt.varRef.loc))
      }
    }

    exprOpt <- analyzeExpr(assignStmt.expr)

    result <- (varSymOpt, exprOpt) match {
      case (Some(varSym), Some(exprWithType)) =>
        if (TypeSystem.AssignRules.canBeAssigned(varSym.typeSym.spec, exprWithType.typeSym.spec)) {
          SemanticAnalyzer.pure(Some(AssignStmtBoundAstNode(varSym, exprWithType, assignStmt.loc)))
        } else {
          SemanticAnalyzer.reportErrAndMapNone(SemanticErr.CannotAssign(varSym.typeSym, exprWithType.typeSym, assignStmt.loc))
        }
      case _                                  => SemanticAnalyzer.pure(None)
    }
  } yield result
}
private def analyzeIfStmt(ifStmt: AstIfStmt): SemanticAnalyzer[Option[IfStmtBoundAstNode]] = {
  def analyzeOptionalStmt(stmtOpt: Option[AstStmt]): SemanticAnalyzer[Option[StmtBoundAstNode]] =
    stmtOpt match {
      case Some(stmt) => analyzeStmt(stmt)
      case None       => SemanticAnalyzer.pure(Some(CompoundStmtBoundAstNode(List.empty)))
    }

  for {
    exprOpt <- analyzeExpr(ifStmt.condition)
    thenStmtOpt <- analyzeOptionalStmt(ifStmt.thenStmt)
    elseStmtOpt <- analyzeOptionalStmt(ifStmt.elseStmt)

    result <- exprOpt match {
      case None                                         => SemanticAnalyzer.pure(None)
      case Some(TypedExpr(expr, TypeSymbol.BooleanSym)) =>
        (thenStmtOpt, elseStmtOpt) match {
          case (Some(thenStmt), Some(elseStmt)) => SemanticAnalyzer.pure(Some(IfStmtBoundAstNode(
            TypedExpr(expr, TypeSymbol.BooleanSym), ifStmt.condition.loc, thenStmt, elseStmt
          )))
          case _                                => SemanticAnalyzer.pure(None)
        }
      case Some(TypedExpr(_, notBooleanTypeSym))        => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.IncorrectType(TypeSymbol.BooleanSym, notBooleanTypeSym, ifStmt.condition.loc))
    }
  } yield result
}
private def analyzeProcCallStmt(procCallStmt: AstProcCallStmt): SemanticAnalyzer[Option[ProcCallStmtBoundAstNode]] = {
  for {
    procSymOpt <- SemanticAnalyzer.currentScope.flatMap { scope =>
      val (procNameIdent, procNameLoc) = procCallStmt.procName
      scope.lookup(procNameIdent) match {
        case Some(procSym: ProcSymbol) => SemanticAnalyzer.pure(Some(procSym))
        case Some(sym)                 => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedProcSym(sym, procNameLoc))
        case None                      => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredProcSym(procNameIdent, procNameLoc))
      }
    }
    analyzedParamOpts <- procCallStmt.actualParams.traverseAnalyzer(analyzeExpr)
    result <- procSymOpt match {
      case Some(procSym) => analyzeProcCallWithResolvedProc(procCallStmt, procSym, analyzedParamOpts)
      case None          => SemanticAnalyzer.pure(None)
    }
  } yield result
}

private def analyzeProcCallWithResolvedProc(
                                             procCallStmt: AstProcCallStmt,
                                             procSym: ProcSymbol,
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
          case ((_, _), None)                                         => SemanticAnalyzer.pure(false) // all errors are handled in analyzeExpr
          case ((formalParam, actualParamAst), Some(actualParamExpr)) =>
            if (TypeSystem.AssignRules.canBeAssigned(formalParam.typeSym.spec, actualParamExpr.typeSym.spec)) {
              SemanticAnalyzer.pure(true)
            } else {
              SemanticAnalyzer
                .reportErr(SemanticErr.CannotPassAsParam(formalParam.typeSym, actualParamExpr.typeSym, actualParamAst.loc))
                .map(_ => false)
            }
        })
      result = {
        val allExprsWereAnalyzed = analyzedParamOpts.forall(_.isDefined)
        val allTypesAreCorrect = paramTypeChecks.forall(identity)
        if (allExprsWereAnalyzed && allTypesAreCorrect) {
          Some(ProcCallStmtBoundAstNode(procSym, analyzedParamOpts.flatten, procCallStmt.loc))
        } else None
      }
    } yield result
  }
}
//expressions
private def analyzeExpr(expr: AstExpr): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  expr match {
    case l: AstBooleanLiteral => SemanticAnalyzer.pure(Some(TypedExpr(BooleanLiteralBoundAstNode(l.value), TypeSymbol.BooleanSym)))
    case l: AstRealLiteral    => SemanticAnalyzer.pure(Some(TypedExpr(RealLiteralBoundAstNode(l.value), TypeSymbol.RealSym)))
    case l: AstIntegerLiteral => SemanticAnalyzer.pure(Some(TypedExpr(IntegerLiteralBoundAstNode(l.value), TypeSymbol.IntegerSym)))
    case l: AstStringLiteral  => SemanticAnalyzer.pure(Some(TypedExpr(StringLiteralBoundAstNode(l.value), TypeSymbol.StringSym)))
    case varRef: AstVarRef    => analyzeVarRef(varRef)
    case unOp: AstUnOp        => analyzeUnOp(unOp)
    case binOp: AstBinOp      => analyzeBinOp(binOp)
  }
}
private def analyzeVarRef(varRef: AstVarRef): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  val varIdent = varRef.ident
  SemanticAnalyzer.currentScope.flatMap {
    scope =>
      scope.lookup(varIdent) match {
        case Some(varSym: VarSymbol) => SemanticAnalyzer.pure(Some(TypedExpr(
          VarRefBoundAstNode(varSym, varRef.loc), varSym.typeSym
        )))
        case Some(sym)               => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedVarSym(sym, varRef.loc))
        case None                    => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndeclaredVarSym(varIdent, varRef.loc))
      }

  }
}
private def analyzeUnOp(node: AstUnOp): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  for {
    innerExpr <- analyzeExpr(node.expr)
    result <- innerExpr match {
      case Some(TypedExpr(innerExpr, exprType)) =>
        TypeSystem.UnOpRules.inferResultType(exprType.spec, node.op._1) match {
          case None             => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.InvalidUnOp(
            node.op._1, exprType, node.op._2
          ))
          case Some(resultType) => SemanticAnalyzer.pure(Some(TypedExpr(
            UnOpBoundAstNode(innerExpr, node.op._1, node.loc), TypeSymbol.fromType(resultType)
          )))
        }
      case _                                    => SemanticAnalyzer.pure(None)
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
          case None             => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.InvalidBinOp(
            node.op._1, lType, rType, node.op._2
          ))
          case Some(resultType) => SemanticAnalyzer.pure(Some(TypedExpr(
            BinOpBoundAstNode(lExpr, node.op._1, rExpr, node.loc),
            TypeSymbol.fromType(resultType)
          )))
        }
      case _                                                              => SemanticAnalyzer.pure(None)
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