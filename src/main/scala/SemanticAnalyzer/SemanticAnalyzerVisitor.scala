package SemanticAnalyzer

import Lexer.Loc
import Parser.*

def analyzeAst(str: String): Either[ParserErr | List[SemanticErr], BoundAstRoot] = {
  constructAst(str) match {
    case Left(err) => Left(err)
    case Right(ast) =>
      val (boundAstRoot, finalCtx) = analyzeAstRoot(ast).run(SemanticCtx.init)
      val errors = finalCtx.errors.toList

      if (errors.nonEmpty)
        Left(errors)
      else
        Right(boundAstRoot)
  }
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
  } yield BlockBoundAstNode(
    varDecls = varDecls,
    procDecls = procDecls,
    compoundStmt = compoundStmt
  )

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
    val typeName = typeAnnotation._1.value
    val typeLoc = typeAnnotation._2
    scope.lookup(typeName) match {
      case Some(typeSym: TypeSymbol) => SemanticAnalyzer.pure(Some(typeSym))
      case Some(received) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedTypeSym(received, typeLoc))
      case None => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndefinedTypeSym(typeName, typeLoc))
    }
  }
}

private def analyzeVarDeclInGroup(
                                   varRef: AstVarRef, typeSymOpt: Option[TypeSymbol]
                                 ): SemanticAnalyzer[Option[VarDeclBoundAstNode]] = {
  SemanticAnalyzer.currentScope.flatMap { scope =>
    val varName = varRef.ident.value
    (scope.lookupLocal(varName), typeSymOpt) match {
      case (Some(alreadyDeclared), _) =>
        SemanticAnalyzer.reportErrAndMapNone(SemanticErr.SymAlreadyDeclared(varRef.loc, alreadyDeclared))
      case (None, Some(typeSym)) => {
        val varSym = VariableSymbol(varRef.ident, typeSym, UserDeclOrigin(varRef.loc))
        SemanticAnalyzer
          .addSymbolToCurrentScope(varName, varSym)
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
    case compoundStmt: AstCompoundStmt => analyzeCompoundStmt(compoundStmt).map(Some(_))
    case assignStmt: AstAssignStmt => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndefinedVarSym(assignStmt.varRef.ident.value, assignStmt.varRef.loc))
    case _ => SemanticAnalyzer.pure(None)
  }
}
private def analyzeCompoundStmt(compoundStmt: AstCompoundStmt): SemanticAnalyzer[CompoundStmtBoundAstNode] = {
  for {
    checkedStmts <- compoundStmt.stmts.traverseAnalyzerOpt(analyzeStmt)
  } yield CompoundStmtBoundAstNode(checkedStmts)
}
private def canBeAssigned(targetType: TypeSymbol, exprType: TypeSymbol): Boolean = ???
private def analyzeAssignStmt(assignStmt: AstAssignStmt): SemanticAnalyzer[Option[AssignStmtBoundAstNode]] = {
  for {
    varSymOpt <- SemanticAnalyzer.currentScope.flatMap { scope =>
      val varName = assignStmt.varRef.ident.value
      scope.lookup(varName) match {
        case Some(varSym: VariableSymbol) => SemanticAnalyzer.pure(Some(varSym))
        case Some(sym) => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.ExpectedVarSym(sym, assignStmt.varRef.loc))
        case None => SemanticAnalyzer.reportErrAndMapNone(SemanticErr.UndefinedVarSym(varName, assignStmt.varRef.loc))
      }
    }

    exprOpt <- analyzeExpr(assignStmt.expr)

    result <- (varSymOpt, exprOpt) match {
      case (Some(varSym), Some(expr)) =>
        if (canBeAssigned(varSym.typeSym, expr.typeSym)) {
          SemanticAnalyzer.pure(Some(AssignStmtBoundAstNode(varSym = varSym, expr = expr)))
        } else {
          SemanticAnalyzer.reportErrAndMapNone(SemanticErr.CannotAssign(varSym.typeSym, expr.typeSym, assignStmt.loc))
        }
      case _ => SemanticAnalyzer.pure(None)
    }
  } yield result
}
//expressions
private def analyzeExpr(expr: AstExpr): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  expr match {
    case l: AstBooleanLiteral => SemanticAnalyzer.pure(Some(TypedExpr(BooleanLiteralBoundAstNode(l.value), TypeSymbol.BooleanSym)))
    case l: AstRealLiteral => SemanticAnalyzer.pure(Some(TypedExpr(RealLiteralBoundAstNode(l.value), TypeSymbol.RealSym)))
    case l: AstIntegerLiteral => SemanticAnalyzer.pure(Some(TypedExpr(IntegerLiteralBoundAstNode(l.value), TypeSymbol.IntegerSym)))
    case l: AstStringLiteral => SemanticAnalyzer.pure(Some(TypedExpr(StringLiteralBoundAstNode(l.value), TypeSymbol.StringSym)))
    case _ => ???
  }
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