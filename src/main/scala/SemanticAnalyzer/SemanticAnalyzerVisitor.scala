package SemanticAnalyzer

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

private def analyzeVarDecls(decls: List[AstVarDecl]): SemanticAnalyzer[List[VarDeclBoundAstNode]] =
  SemanticAnalyzer.pure(List.empty)
private def analyzeProcDecls(decls: List[AstProcDecl]): SemanticAnalyzer[List[ProcDeclBoundAstNode]] =
  SemanticAnalyzer.pure(List.empty)

private def analyzeCompoundStmt(compoundStmt: AstCompoundStmt): SemanticAnalyzer[CompoundStmtBoundAstNode] = {
  for {
    checkedStmts <- compoundStmt.stmts.traverseAnalyzerOpt(analyzeStmt)
  } yield CompoundStmtBoundAstNode(checkedStmts)
}
private def analyzeStmt(stmt: AstStmt): SemanticAnalyzer[Option[StmtBoundAstNode]] =
  stmt match {
    case assignStmt: AstAssignStmt => SemanticAnalyzer.reportErrAndMapNone(
      SemanticErr.UndefinedVarSym((assignStmt.varRef.ident, assignStmt.varRef.loc))
    )
    case compoundStmt: AstCompoundStmt => analyzeCompoundStmt(compoundStmt).map(Some(_))
    case _ => SemanticAnalyzer.pure(None)
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

  private def traverseAnalyzerOpt[B](f: A => SemanticAnalyzer[Option[B]]): SemanticAnalyzer[List[B]] =
    items.traverseAnalyzer(f).map(_.flatten)
}