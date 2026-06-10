import Parser.*
import SemanticAnalyzer.*
import munit.FunSuite

import java.io.StringReader

final class SemanticAnalyzerSuite extends FunSuite {

  private def parseOk(code: String): AstRoot = {
    _root_.Parser.constructAst(new StringReader(code)) match {
      case Right(ast) => ast
      case Left(err)  => fail(s"Parser failed before semantic analysis: $err")
    }
  }

  private def analyze(code: String): Either[List[SemanticErr], BoundAstRoot] =
    analyzeProgramAst(parseOk(code))

  private def analyzeOk(code: String): BoundAstRoot = {
    analyze(code) match {
      case Right(boundAst) => boundAst
      case Left(errs)      => fail(s"Expected semantic success, but got:\n${errs.mkString("\n")}")
    }
  }

  private def analyzeFail(code: String): List[SemanticErr] = {
    analyze(code) match {
      case Left(errs)      => errs
      case Right(boundAst) => fail(s"Expected semantic errors, but got bound AST: $boundAst")
    }
  }

  private def assertSemanticErrorContains(code: String, expectedPart: String): Unit = {
    val errs = analyzeFail(code)
    val text = errs.mkString("\n")
    assert(text.contains(expectedPart), s"Expected '$expectedPart' inside semantic errors:\n$text")
  }

  test("accepts empty program") {
    analyzeOk(
      """
        |program Main;
        |begin
        |end.
        |""".stripMargin
    )
  }

  test("accepts declarations of all built-in types") {
    val boundAstText = analyzeOk(
      """
        |program Main;
        |var
        |  i : integer;
        |  r : real;
        |  b : boolean;
        |  s : string;
        |begin
        |end.
        |""".stripMargin
    ).toString

    assert(boundAstText.contains("i"))
    assert(boundAstText.contains("r"))
    assert(boundAstText.contains("b"))
    assert(boundAstText.contains("s"))
  }

  test("accepts integer expression assigned to integer") {
    analyzeOk(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 1 + 2 * 3 - 4 div 2;
        |end.
        |""".stripMargin
    )
  }

  test("accepts integer expression assigned to real") {
    analyzeOk(
      """
        |program Main;
        |var x : real;
        |begin
        |  x := 10;
        |end.
        |""".stripMargin
    )
  }

  test("rejects real expression assigned to integer") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 10.5;
        |end.
        |""".stripMargin,
      "CannotAssign"
    )
  }

  test("rejects boolean expression assigned to integer") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := true;
        |end.
        |""".stripMargin,
      "CannotAssign"
    )
  }

  test("rejects use of undeclared variable") {
    assertSemanticErrorContains(
      """
        |program Main;
        |begin
        |  x := 1;
        |end.
        |""".stripMargin,
      "UndeclaredValueSym"
    )
  }

  test("rejects reading undeclared variable in expression") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var y : integer;
        |begin
        |  y := x + 1;
        |end.
        |""".stripMargin,
      "UndeclaredValueSym"
    )
  }

  test("rejects duplicate variables in one scope") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var
        |  x : integer;
        |  x : real;
        |begin
        |end.
        |""".stripMargin,
      "SymAlreadyDeclared"
    )
  }

  test("rejects duplicate variables inside one declaration group") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var x, x : integer;
        |begin
        |end.
        |""".stripMargin,
      "SymAlreadyDeclared"
    )
  }

  test("accepts variable shadowing in nested procedure") {
    analyzeOk(
      """
        |program Main;
        |var x : integer;
        |
        |procedure P;
        |var x : real;
        |begin
        |  x := 1.5;
        |end;
        |
        |begin
        |  x := 1;
        |  P();
        |end.
        |""".stripMargin
    )
  }

  test("accepts procedure access to global variable") {
    analyzeOk(
      """
        |program Main;
        |var x : integer;
        |
        |procedure P;
        |begin
        |  x := x + 1;
        |end;
        |
        |begin
        |  x := 0;
        |  P();
        |end.
        |""".stripMargin
    )
  }

  test("rejects access to procedure local variable outside procedure") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |procedure P;
        |var x : integer;
        |begin
        |  x := 1;
        |end;
        |
        |begin
        |  x := 2;
        |end.
        |""".stripMargin,
      "UndeclaredValueSym"
    )
  }

  test("accepts procedure parameters as local symbols") {
    analyzeOk(
      """
        |program Main;
        |
        |procedure AddOne(x : integer);
        |var y : integer;
        |begin
        |  y := x + 1;
        |end;
        |
        |begin
        |  AddOne(10);
        |end.
        |""".stripMargin
    )
  }

  test("rejects duplicate procedure parameters") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |procedure Bad(x : integer; x : real);
        |begin
        |end;
        |
        |begin
        |end.
        |""".stripMargin,
      "SymAlreadyDeclared"
    )
  }

  test("rejects procedure redeclaration in same scope") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |procedure P;
        |begin
        |end;
        |
        |procedure P;
        |begin
        |end;
        |
        |begin
        |end.
        |""".stripMargin,
      "SymAlreadyDeclared"
    )
  }

  test("accepts procedure call with correct argument count and types") {
    analyzeOk(
      """
        |program Main;
        |
        |procedure P(a : integer; b : real; c : string);
        |begin
        |end;
        |
        |begin
        |  P(1, 2, "ok");
        |end.
        |""".stripMargin
    )
  }

  test("rejects procedure call with too few arguments") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |procedure P(a : integer; b : integer);
        |begin
        |end;
        |
        |begin
        |  P(1);
        |end.
        |""".stripMargin,
      "IncorrectActualParamsCount"
    )
  }

  test("rejects procedure call with too many arguments") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |procedure P(a : integer);
        |begin
        |end;
        |
        |begin
        |  P(1, 2);
        |end.
        |""".stripMargin,
      "IncorrectActualParamsCount"
    )
  }

  test("rejects procedure call with wrong argument type") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |procedure P(a : integer);
        |begin
        |end;
        |
        |begin
        |  P("not integer");
        |end.
        |""".stripMargin,
      "CannotPassAsParam"
    )
  }

  test("accepts integer actual argument for real formal parameter") {
    analyzeOk(
      """
        |program Main;
        |
        |procedure P(x : real);
        |begin
        |end;
        |
        |begin
        |  P(10);
        |end.
        |""".stripMargin
    )
  }

  test("rejects using variable as procedure") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var P : integer;
        |begin
        |  P();
        |end.
        |""".stripMargin,
      "ExpectedProc"
    )
  }

  test("rejects undeclared procedure call") {
    assertSemanticErrorContains(
      """
        |program Main;
        |begin
        |  P();
        |end.
        |""".stripMargin,
      "UndeclaredProc"
    )
  }

  test("accepts boolean condition in if statement") {
    analyzeOk(
      """
        |program Main;
        |var x : integer;
        |begin
        |  if x = 0 then
        |    x := 1
        |  else
        |    x := 2;
        |end.
        |""".stripMargin
    )
  }

  test("rejects integer condition in if statement") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var x : integer;
        |begin
        |  if x then
        |    x := 1;
        |end.
        |""".stripMargin,
      "IncorrectType"
    )
  }

  test("rejects invalid binary operation") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 1 + true;
        |end.
        |""".stripMargin,
      "InvalidBinOp"
    )
  }

  test("rejects invalid unary operation") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := not 1;
        |end.
        |""".stripMargin,
      "InvalidUnOp"
    )
  }

  test("accepts string equality") {
    analyzeOk(
      """
        |program Main;
        |var ok : boolean;
        |begin
        |  ok := "a" = "a";
        |end.
        |""".stripMargin
    )
  }

  test("accepts string concatenation if supported by type system") {
    analyzeOk(
      """
        |program Main;
        |var s : string;
        |begin
        |  s := "a" + "b";
        |end.
        |""".stripMargin
    )
  }

  test("rejects mod with real operand") {
    assertSemanticErrorContains(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 10.0 mod 3;
        |end.
        |""".stripMargin,
      "InvalidBinOp"
    )
  }

  test("accepts function declaration and function call") {
    analyzeOk(
      """
        |program Main;
        |
        |function Add(a : integer; b : integer) : integer;
        |begin
        |  result := a + b;
        |end;
        |
        |var x : integer;
        |
        |begin
        |  x := Add(1, 2);
        |end.
        |""".stripMargin
    )
  }

  test("accepts recursive function") {
    analyzeOk(
      """
        |program Main;
        |
        |function Factorial(n : integer) : integer;
        |begin
        |  if n <= 1 then
        |    result := 1
        |  else
        |    result := n * Factorial(n - 1);
        |end;
        |
        |var x : integer;
        |
        |begin
        |  x := Factorial(5);
        |end.
        |""".stripMargin
    )
  }

  test("rejects function result assigned wrong type") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |function Bad : integer;
        |begin
        |  result := "wrong";
        |end;
        |
        |begin
        |end.
        |""".stripMargin,
      "CannotAssign"
    )
  }

  test("rejects using procedure as expression") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |procedure P;
        |begin
        |end;
        |
        |var x : integer;
        |
        |begin
        |  x := P();
        |end.
        |""".stripMargin,
      "ExpectedFunc"
    )
  }

  test("rejects using function as statement procedure call") {
    assertSemanticErrorContains(
      """
        |program Main;
        |
        |function F : integer;
        |begin
        |  result := 1;
        |end;
        |
        |begin
        |  F();
        |end.
        |""".stripMargin,
      "ExpectedProc"
    )
  }

  test("collects several semantic errors in one pass") {
    val errs = analyzeFail(
      """
        |program Main;
        |var
        |  x : integer;
        |  x : real;
        |begin
        |  y := true + 1;
        |  UnknownProc();
        |end.
        |""".stripMargin
    )

    assert(errs.length >= 2)
    val text = errs.mkString("\n")
    assert(text.contains("SymAlreadyDeclared"))
    assert(text.contains("Undeclared"))
  }

  test("accepts stdlib procedures and functions as predefined symbols") {
    analyzeOk(
      """
        |program Main;
        |var text : string; x : integer;
        |begin
        |  x := stringLength("abc");
        |  text := convertIntegerToString(x);
        |  printLine(text);
        |end.
        |""".stripMargin
    )
  }
}
