package SemanticAnalyzer.visitor

import Lexer.Loc
import Parser.{AstExpr, AstStmt}
import SemanticAnalyzer.*


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

private def analyzeActualParams(
                                 formalParams: List[ParamSymbol],
                                 actualParamAsts: List[AstExpr],
                                 analyzedParamOpts: List[Option[AnyTypedExpr]],
                                 callLoc: Loc,
                                 incorrectCountErr: Int => SemanticErr
                               ): SemanticAnalyzer[Option[List[AnyTypedExpr]]] = {
  val actualParamsCount = actualParamAsts.length

  if (formalParams.length != actualParamsCount) {
    SemanticAnalyzer.reportErrAndMapNone(incorrectCountErr(actualParamsCount))
  } else {
    for {
      paramTypeChecks <- formalParams
        .zip(actualParamAsts)
        .zip(analyzedParamOpts)
        .traverseAnalyzer {
          case (_, None)                                              => SemanticAnalyzer.pure(false)
          case ((formalParam, actualParamAst), Some(actualParamExpr)) => {
            if (TypeSystem.AssignRules.canBeAssigned(formalParam.typeSym.spec, actualParamExpr.typeSym.spec)) {
              SemanticAnalyzer.pure(true)
            } else {
              SemanticAnalyzer.reportErr(SemanticErr.CannotPassAsParam(
                target = formalParam.typeSym,
                received = actualParamExpr.typeSym,
                actualParamLoc = actualParamAst.loc
              )).map(_ => false)
            }
          }
        }

      result = {
        val allExprsWereAnalyzed = analyzedParamOpts.forall(_.isDefined)
        val allTypesAreCorrect = paramTypeChecks.forall(identity)

        if (allExprsWereAnalyzed && allTypesAreCorrect) {
          Some(analyzedParamOpts.flatten)
        } else {
          None
        }
      }
    } yield result
  }
}