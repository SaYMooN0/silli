import Interpreter.{IOCtx, InterpretationSuccess, RuntimeErr}
import Parser.ParserErr
import SemanticAnalyzer.BoundAstRoot

import scala.io.Source

@main
def main(): Unit = {
  val input = Source.fromFile("pascalProgram").mkString
  val ioCtx = IOCtx.createForConsole()

  val interpreterResult: Either[FailedInterpretationFlow, InterpretationSuccess.type] = Parser.constructAst(input) match {
    case Left(err) => Left(FailedInterpretationFlow.ParserErr(err))
    case Right(ast) => SemanticAnalyzer.analyzeProgramAst(ast) match {
      case Left(semanticErrs) => Left(FailedInterpretationFlow.SemanticErrs(semanticErrs))
      case Right(ast: BoundAstRoot) => Interpreter.interpretBoundAst(ast, ioCtx) match {
        case Left(runtimeErr: RuntimeErr) => Left(FailedInterpretationFlow.RuntimeErr(runtimeErr))
        case Right(success) => Right(success)
      }
    }
  }
  val strToPrint = interpreterResult match {
    case Left(ff) => FailedInterpretationFlow.toString(ff)
    case Right(_) => "Success"
  }
  println(strToPrint)
}


private enum FailedInterpretationFlow {
  case ParserErr(err: Parser.ParserErr)
  case SemanticErrs(errs: List[SemanticAnalyzer.SemanticErr])
  case RuntimeErr(err: Interpreter.RuntimeErr)
}

object FailedInterpretationFlow {
  private def toStringParserErr(e: Parser.ParserErr) = e.toString

  private def toStringSemanticErr(e: SemanticAnalyzer.SemanticErr) = e.toString

  private def toStringRuntimeErr(e: Interpreter.RuntimeErr) = e.toString

  def toString(failedFlow: FailedInterpretationFlow): String = failedFlow match {
    case parserErr: ParserErr => toStringParserErr(parserErr.err)
    case semanticErrs: SemanticErrs => semanticErrs.errs.map(e => toStringSemanticErr(e)).mkString("\n")
    case runtimeErr: RuntimeErr => toStringRuntimeErr(runtimeErr.err)
  }
}