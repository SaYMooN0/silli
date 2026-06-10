import Interpreter.{FailedInterpretationFlow, IOCtx}

import java.io.StringReader
import scala.io.{Source, StdIn}

object Main {
  def main(args: Array[String]): Unit = {
    val filePath =
      if (args.nonEmpty) args(0)
      else "pascalProgram"

    val source = Source.fromFile(filePath)


    try {
      val input = new StringReader(source.mkString)
      val ioCtx = IOCtx.createForConsole()

      val interpreterResult = Interpreter.runInterpreter(input, ioCtx)
      val strToPrint = interpreterResult match {
        case Left(ff) => ErrStringifier.errToString(ff)
        case Right(_) => "Success"
      }

      println(strToPrint)
    } finally {
      source.close()
    }
  }
}

private object ErrStringifier {
  private def toStringParserErr(e: Parser.ParserErr): String = e.toString

  private def toStringSemanticErr(e: SemanticAnalyzer.SemanticErr): String = e.toString

  private def toStringRuntimeErr(e: Interpreter.RuntimeErr): String = e.toString

  def errToString(failedFlow: FailedInterpretationFlow): String =
    failedFlow match {
      case parserErr: FailedInterpretationFlow.ParserErr       => toStringParserErr(parserErr.err)
      case semanticErrs: FailedInterpretationFlow.SemanticErrs => semanticErrs.errs.map(toStringSemanticErr).mkString("\n")
      case runtimeErr: FailedInterpretationFlow.RuntimeErr     => toStringRuntimeErr(runtimeErr.err)
    }
}

object IOCtx {
  def createForConsole(): IOCtx = new IOCtx {
    override def readLine(): String =
      StdIn.readLine()

    override def readSingleCharAsAsciiCode(): Int =
      System.in.read()

    override def write(value: String): Unit =
      print(value)
  }
}