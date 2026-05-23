import scala.io.Source

@main
def main(): Unit = {
  val input = Source.fromFile("pascalProgram").mkString
  println(SemanticAnalyzer.analyzeAst(input))
}

