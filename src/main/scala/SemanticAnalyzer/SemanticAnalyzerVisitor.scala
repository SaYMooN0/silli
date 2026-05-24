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
    case _ => SemanticAnalyzer.pure(None)
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
private def analyzeUnOp(node: AstUnOp): SemanticAnalyzer[Option[AnyTypedExpr]] = ???
private def analyzeBinOp(node: AstBinOp): SemanticAnalyzer[Option[AnyTypedExpr]] = {
  for {
    leftExpr <- analyzeExpr(node.left)
    rightExpr <- analyzeExpr(node.right)
    result <- (leftExpr, rightExpr) match {
      case (Some(TypedExpr(lExpr, lType)), Some(TypedExpr(rExpr, rType))) =>
        TypeSystem.inferBinOpResultType(lType.spec, node.op._1, rType.spec) match {
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