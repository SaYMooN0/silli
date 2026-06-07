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
    decls <- visitor.analyzeBlockDecls(block.decls)
    compoundStmt <- visitor.analyzeCompoundStmt(block.compoundStmt)
  } yield BlockBoundAstNode(decls, compoundStmt)
}
