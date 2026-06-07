package SemanticAnalyzer.visitor

import Lexer.Loc
import Parser.*
import SemanticAnalyzer.*

private[SemanticAnalyzer] def analyzeBlockDecls(
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
                                   varRef: AstExpr.VarRef,
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

      case None => for {
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

        _ <- if (funcResultWasAssigned(block, resultSym)) {
          SemanticAnalyzer.pure(())
        } else {
          SemanticAnalyzer.reportErr(SemanticErr.FuncResultNotAssigned(funcSym, funcIdentLoc))
        }
      } yield Some(DeclItemBoundAstNode.FuncDecl(funcSym, funcIdentLoc, block))
    }
  }
}
private def funcResultWasAssigned(block: BlockBoundAstNode, resultSym: FuncResultSymbol): Boolean =
  stmtAssignsFuncResult(block.compoundStmt, resultSym)

private def stmtAssignsFuncResult(stmt: StmtBoundAstNode, resultSym: FuncResultSymbol): Boolean =
  stmt match {
    case StmtBoundAstNode.AssignStmt(valueSymbol, _, _) => valueSymbol == resultSym

    case StmtBoundAstNode.CompoundStmt(stmts) => stmts.exists(stmtAssignsFuncResult(_, resultSym))

    case StmtBoundAstNode.IfStmt(_, _, thenStmt, elseStmt) =>
      stmtAssignsFuncResult(thenStmt, resultSym)
        || stmtAssignsFuncResult(elseStmt, resultSym)

    case StmtBoundAstNode.ProcCall(_, _, _) => false
  }

private def analyzeRoutineFormalParams(
                                        params: List[AstFormalParam]
                                      ): SemanticAnalyzer[List[Option[ParamSymbol]]] =
  params.foldLeft(SemanticAnalyzer.pure(List.empty[Option[ParamSymbol]])) {
    case (acc, param) =>
      for {
        gathered <- acc
        analyzed <- analyzeRoutineFormalParam(param)
      } yield gathered :+ analyzed
  }

private def analyzeRoutineFormalParam(
                                       param: AstFormalParam
                                     ): SemanticAnalyzer[Option[ParamSymbol]] =
  SemanticAnalyzer.currentScope.flatMap { scope =>
    val (paramIdent, paramIdentLoc) = param.varRef

    analyzeTypeSpec(param.typeAnnotation).flatMap { typeSymOpt =>
      (scope.lookupLocal(paramIdent), typeSymOpt) match {
        case (Some(alreadyDeclared), _) =>
          SemanticAnalyzer.reportErrAndMapNone(
            SemanticErr.SymAlreadyDeclared(paramIdentLoc, alreadyDeclared)
          )

        case (None, Some(typeSym)) =>
          val paramSym = ParamSymbol(paramIdent, typeSym, UserDeclOrigin(paramIdentLoc))

          SemanticAnalyzer
            .addSymbolToCurrentScope(paramIdent, paramSym)
            .map(_ => Some(paramSym))

        case (None, None) => SemanticAnalyzer.pure(None)
      }
    }
  }