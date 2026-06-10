import Parser.*
import munit.FunSuite

import java.io.StringReader

final class ParserSuite extends FunSuite {

  private def parseOk(code: String): String = {
    _root_.Parser.constructAst(new StringReader(code)) match {
      case Right(ast) => ast.toString
      case Left(err)  => fail(s"Expected parser success, but got: $err")
    }
  }

  private def parseFail(code: String): ParserErr = {
    _root_.Parser.constructAst(new StringReader(code)) match {
      case Left(err)  => err
      case Right(ast) => fail(s"Expected parser error, but got AST: $ast")
    }
  }

  private def assertParseFailsWith(code: String, expectedPart: String): Unit = {
    val err = parseFail(code)
    assert(err.toString.contains(expectedPart), s"Expected '$expectedPart' inside '$err'")
  }

  test("parses minimal empty program") {
    val astText = parseOk(
      """
        |program Main;
        |begin
        |end.
        |""".stripMargin
    )

    assert(astText.contains("Main"))
  }

  test("parses variable declarations with several built-in types") {
    val astText = parseOk(
      """
        |program Main;
        |var
        |  a, b : integer;
        |  c : real;
        |  flag : boolean;
        |  text : string;
        |begin
        |end.
        |""".stripMargin
    )

    assert(astText.contains("integer"))
    assert(astText.contains("real"))
    assert(astText.contains("boolean"))
    assert(astText.contains("string"))
  }

  test("parses assignment statements") {
    val astText = parseOk(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 1;
        |  x := x + 2;
        |end.
        |""".stripMargin
    )

    assert(astText.contains("x"))
    assert(astText.contains("Add") || astText.contains("Plus"))
  }

  test("parses procedure declaration without parameters") {
    val astText = parseOk(
      """
        |program Main;
        |
        |procedure Hello;
        |begin
        |end;
        |
        |begin
        |  Hello();
        |end.
        |""".stripMargin
    )

    assert(astText.contains("Hello"))
  }

  test("parses procedure declaration with grouped formal parameters") {
    val astText = parseOk(
      """
        |program Main;
        |
        |procedure AddAndPrint(a, b : integer; labelText : string);
        |begin
        |end;
        |
        |begin
        |  AddAndPrint(1, 2, "sum");
        |end.
        |""".stripMargin
    )

    assert(astText.contains("AddAndPrint"))
    assert(astText.contains("labelText"))
  }

  test("parses nested procedure declaration") {
    val astText = parseOk(
      """
        |program Main;
        |
        |procedure Outer;
        |  procedure Inner;
        |  begin
        |  end;
        |begin
        |  Inner();
        |end;
        |
        |begin
        |  Outer();
        |end.
        |""".stripMargin
    )

    assert(astText.contains("Outer"))
    assert(astText.contains("Inner"))
  }

  test("parses function declaration and call expression") {
    val astText = parseOk(
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
        |  x := Add(3, 5);
        |end.
        |""".stripMargin
    )

    assert(astText.contains("Add"))
    assert(astText.contains("result"))
  }

  test("parses nested function calls in expression") {
    val astText = parseOk(
      """
        |program Main;
        |
        |function Add(a : integer; b : integer) : integer;
        |begin
        |  result := a + b;
        |end;
        |
        |function DoubleValue(x : integer) : integer;
        |begin
        |  result := x * 2;
        |end;
        |
        |var y : integer;
        |
        |begin
        |  y := DoubleValue(Add(1, 2));
        |end.
        |""".stripMargin
    )

    assert(astText.contains("DoubleValue"))
    assert(astText.contains("Add"))
  }

  test("parses if statement with then and else branches") {
    val astText = parseOk(
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

    assert(astText.contains("If"))
    assert(astText.contains("x"))
  }

  test("parses if statement with empty then branch") {
    val astText = parseOk(
      """
        |program Main;
        |var x : integer;
        |begin
        |  if x = 0 then;
        |end.
        |""".stripMargin
    )

    assert(astText.contains("If"))
  }

  test("parses nested compound statements") {
    val astText = parseOk(
      """
        |program Main;
        |var x : integer;
        |begin
        |  begin
        |    x := 1;
        |    x := 2;
        |  end;
        |end.
        |""".stripMargin
    )

    assert(astText.contains("Compound"))
  }

  test("parses expression precedence: multiplication before addition") {
    val astText = parseOk(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 1 + 2 * 3;
        |end.
        |""".stripMargin
    )

    assert(astText.contains("Add") || astText.contains("Plus"))
    assert(astText.contains("Mul"))
  }

  test("parses expression precedence with parentheses") {
    val astText = parseOk(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := (1 + 2) * 3;
        |end.
        |""".stripMargin
    )

    assert(astText.contains("Add") || astText.contains("Plus"))
    assert(astText.contains("Mul"))
  }

  test("parses unary operators") {
    val astText = parseOk(
      """
        |program Main;
        |var x : integer; flag : boolean;
        |begin
        |  x := -1;
        |  x := +x;
        |  flag := not false;
        |end.
        |""".stripMargin
    )

    assert(astText.contains("Minus"))
    assert(astText.contains("Not"))
  }

  test("parses logical and comparison expressions") {
    val astText = parseOk(
      """
        |program Main;
        |var a : integer; b : integer; ok : boolean;
        |begin
        |  ok := a < b and b >= 0 or a <> 10;
        |end.
        |""".stripMargin
    )

    assert(astText.contains("And"))
    assert(astText.contains("Or"))
  }

  test("parses many semicolons between statements") {
    val astText = parseOk(
      """
        |program Main;
        |var x : integer;
        |begin
        |  ;;;
        |  x := 1;;;;
        |  x := 2;;;
        |end.
        |""".stripMargin
    )

    assert(astText.contains("x"))
  }

  test("fails when program keyword is missing") {
    assertParseFailsWith(
      """
        |Main;
        |begin
        |end.
        |""".stripMargin,
      "UnexpectedToken"
    )
  }

  test("fails when semicolon after program name is missing") {
    assertParseFailsWith(
      """
        |program Main
        |begin
        |end.
        |""".stripMargin,
      "UnexpectedToken"
    )
  }

  test("fails when final dot is missing") {
    assertParseFailsWith(
      """
        |program Main;
        |begin
        |end
        |""".stripMargin,
      "Unexpected"
    )
  }

  test("fails on broken variable declaration") {
    assertParseFailsWith(
      """
        |program Main;
        |var x integer;
        |begin
        |end.
        |""".stripMargin,
      "UnexpectedToken"
    )
  }

  test("fails on missing right parenthesis in call") {
    assertParseFailsWith(
      """
        |program Main;
        |begin
        |  P(1, 2;
        |end.
        |""".stripMargin,
      "UnexpectedToken"
    )
  }

  test("fails on invalid primary expression") {
    assertParseFailsWith(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := + ;
        |end.
        |""".stripMargin,
      "CouldNotParse"
    )
  }

  test("fails on broken if statement") {
    assertParseFailsWith(
      """
        |program Main;
        |begin
        |  if true begin end;
        |end.
        |""".stripMargin,
      "UnexpectedToken"
    )
  }

  test("fails on unexpected extra token after dot") {
    assertParseFailsWith(
      """
        |program Main;
        |begin
        |end. garbage
        |""".stripMargin,
      "ExpectedEnd"
    )
  }
}
