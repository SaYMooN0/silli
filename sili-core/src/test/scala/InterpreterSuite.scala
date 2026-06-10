import Interpreter.*
import munit.FunSuite
import testutils.TestIOCtx

import java.io.StringReader

final class InterpreterSuite extends FunSuite {

  private val nl: String = System.lineSeparator()

  private def runProgram(code: String, io: TestIOCtx = TestIOCtx.empty): TestIOCtx = {
    _root_.Interpreter.runInterpreter(new StringReader(code), io) match {
      case Right(_)  => io
      case Left(err) => fail(s"Expected successful interpretation, but got: $err")
    }
  }

  private def runProgramExpectError(code: String, io: TestIOCtx = TestIOCtx.empty): String = {
    _root_.Interpreter.runInterpreter(new StringReader(code), io) match {
      case Left(err) => err.toString
      case Right(_)  => fail(s"Expected interpretation error, but program succeeded. Output: ${io.written}")
    }
  }

  test("executes integer arithmetic and printLine") {
    val io = runProgram(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 1 + 2 * 3;
        |  printLine(convertIntegerToString(x));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"7$nl")
  }

  test("executes real arithmetic and convertRealToString") {
    val io = runProgram(
      """
        |program Main;
        |var x : real;
        |begin
        |  x := 10 / 4;
        |  printLine(convertRealToString(x));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"2.5$nl")
  }

  test("executes integer div") {
    val io = runProgram(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 10 div 3;
        |  printLine(convertIntegerToString(x));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"3$nl")
  }

  test("executes boolean expressions") {
    val io = runProgram(
      """
        |program Main;
        |var ok : boolean;
        |begin
        |  ok := true and not false;
        |  printLine(convertBooleanToString(ok));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"true$nl")
  }

  test("executes if true branch") {
    val io = runProgram(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 0;
        |  if x = 0 then
        |    x := 10
        |  else
        |    x := 20;
        |  printLine(convertIntegerToString(x));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"10$nl")
  }

  test("executes if false branch") {
    val io = runProgram(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := 1;
        |  if x = 0 then
        |    x := 10
        |  else
        |    x := 20;
        |  printLine(convertIntegerToString(x));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"20$nl")
  }

  test("executes procedure without parameters") {
    val io = runProgram(
      """
        |program Main;
        |
        |procedure Hello;
        |begin
        |  printLine("hello");
        |end;
        |
        |begin
        |  Hello();
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"hello$nl")
  }

  test("executes procedure with parameters") {
    val io = runProgram(
      """
        |program Main;
        |
        |procedure PrintSum(a : integer; b : integer);
        |var sum : integer;
        |begin
        |  sum := a + b;
        |  printLine(convertIntegerToString(sum));
        |end;
        |
        |begin
        |  PrintSum(3, 5);
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"8$nl")
  }

  test("procedure can modify global variable through static link") {
    val io = runProgram(
      """
        |program Main;
        |var x : integer;
        |
        |procedure Inc;
        |begin
        |  x := x + 1;
        |end;
        |
        |begin
        |  x := 0;
        |  Inc();
        |  Inc();
        |  Inc();
        |  printLine(convertIntegerToString(x));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"3$nl")
  }

  test("local variable shadows global variable") {
    val io = runProgram(
      """
        |program Main;
        |var x : integer;
        |
        |procedure P;
        |var x : integer;
        |begin
        |  x := 100;
        |  printLine(convertIntegerToString(x));
        |end;
        |
        |begin
        |  x := 1;
        |  P();
        |  printLine(convertIntegerToString(x));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"100${nl}1$nl")
  }

  test("nested procedure can access variable from defining procedure") {
    val io = runProgram(
      """
        |program Main;
        |
        |procedure Outer;
        |var x : integer;
        |
        |  procedure Inner;
        |  begin
        |    x := x + 5;
        |  end;
        |
        |begin
        |  x := 10;
        |  Inner();
        |  printLine(convertIntegerToString(x));
        |end;
        |
        |begin
        |  Outer();
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"15$nl")
  }

  test("executes simple function call") {
    val io = runProgram(
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
        |  printLine(convertIntegerToString(x));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"8$nl")
  }

  test("executes nested function calls") {
    val io = runProgram(
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
        |  y := DoubleValue(Add(2, 3));
        |  printLine(convertIntegerToString(y));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"10$nl")
  }

  test("executes recursive factorial function") {
    val io = runProgram(
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
        |var answer : integer;
        |
        |begin
        |  answer := Factorial(5);
        |  printLine(convertIntegerToString(answer));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"120$nl")
  }

  test("coerces integer argument to real formal parameter at runtime") {
    val io = runProgram(
      """
        |program Main;
        |
        |function DoubleValue(x : real) : real;
        |begin
        |  result := x * 2;
        |end;
        |
        |var y : real;
        |
        |begin
        |  y := DoubleValue(10);
        |  printLine(convertRealToString(y));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"20.0$nl")
  }

  test("coerces integer function result assignment to real result variable") {
    val io = runProgram(
      """
        |program Main;
        |
        |function Ten : real;
        |begin
        |  result := 10;
        |end;
        |
        |var y : real;
        |
        |begin
        |  y := Ten();
        |  printLine(convertRealToString(y));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"10.0$nl")
  }

  test("executes readLine through TestIOCtx") {
    val io = TestIOCtx.withLines("Alice")

    runProgram(
      """
        |program Main;
        |var name : string;
        |begin
        |  name := readLine();
        |  printString("Hello, ");
        |  printLine(name);
        |end.
        |""".stripMargin,
      io
    )

    assertEquals(io.written, s"Hello, Alice$nl")
    assertEquals(io.remainingLineInputsCount, 0)
  }

  test("executes readSingleCharAsAsciiCode through TestIOCtx") {
    val io = TestIOCtx.withCharCodes(65)

    runProgram(
      """
        |program Main;
        |var code : integer;
        |begin
        |  code := readSingleCharAsAsciiCode();
        |  printLine(convertIntegerToString(code));
        |end.
        |""".stripMargin,
      io
    )

    assertEquals(io.written, s"65$nl")
  }

  test("readSingleCharAsAsciiCode returns -1 on end of test input") {
    val io = TestIOCtx.empty

    runProgram(
      """
        |program Main;
        |var code : integer;
        |begin
        |  code := readSingleCharAsAsciiCode();
        |  printLine(convertIntegerToString(code));
        |end.
        |""".stripMargin,
      io
    )

    assertEquals(io.written, s"-1$nl")
  }

  test("executes stringLength and stringCharAt") {
    val io = runProgram(
      """
        |program Main;
        |var text : string;
        |begin
        |  text := "abcd";
        |  printLine(convertIntegerToString(stringLength(text)));
        |  printLine(stringCharAt(text, 2));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"4${nl}c$nl")
  }

  test("executes stringLeftUntilIndex and stringRightFromIndex") {
    val io = runProgram(
      """
        |program Main;
        |var text : string;
        |begin
        |  text := "abcdef";
        |  printLine(stringLeftUntilIndex(text, 3));
        |  printLine(stringRightFromIndex(text, 3));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"abc${nl}def$nl")
  }

  test("executes stringSetCharAt") {
    val io = runProgram(
      """
        |program Main;
        |var text : string;
        |begin
        |  text := stringSetCharAt("abc", 1, "X");
        |  printLine(text);
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"aXc$nl")
  }

  test("executes stringRepeat") {
    val io = runProgram(
      """
        |program Main;
        |begin
        |  printLine(stringRepeat("ab", 3));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"ababab$nl")
  }

  test("executes ASCII conversion helpers") {
    val io = runProgram(
      """
        |program Main;
        |var text : string; code : integer;
        |begin
        |  text := stringFromAsciiCode(65);
        |  code := asciiCodeFromString(text);
        |  printLine(text);
        |  printLine(convertIntegerToString(code));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"A${nl}65$nl")
  }

  test("executes real-to-int conversion helpers") {
    val io = runProgram(
      """
        |program Main;
        |begin
        |  printLine(convertIntegerToString(roundRealToInt(10.6)));
        |  printLine(convertIntegerToString(ceilRealToInt(10.1)));
        |  printLine(convertIntegerToString(floorRealToInt(10.9)));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"11${nl}11${nl}10$nl")
  }

  test("reports runtime error on undefined variable access") {
    val err = runProgramExpectError(
      """
        |program Main;
        |var x : integer;
        |begin
        |  printLine(convertIntegerToString(x));
        |end.
        |""".stripMargin
    )

    assert(err.contains("UndefinedVariable") || err.contains("Undefined"))
  }

  test("reports stdlib runtime error on invalid stringCharAt index") {
    val err = runProgramExpectError(
      """
        |program Main;
        |begin
        |  printLine(stringCharAt("abc", 10));
        |end.
        |""".stripMargin
    )

    assert(err.contains("stringCharAt") || err.contains("invalid index"))
  }

  test("runs a small BF-like tape movement program written in Pascal-like language") {
    val io = runProgram(
      """
        |program Main;
        |
        |var
        |  leftTape : string;
        |  rightTape : string;
        |  currentCell : integer;
        |
        |procedure MoveRight;
        |begin
        |  leftTape := leftTape + stringFromAsciiCode(currentCell);
        |
        |  if stringLength(rightTape) = 0 then
        |    currentCell := 0
        |  else
        |  begin
        |    currentCell := asciiCodeFromString(stringCharAt(rightTape, 0));
        |    rightTape := stringRightFromIndex(rightTape, 1);
        |  end;
        |end;
        |
        |procedure MoveLeft;
        |var lastIndex : integer;
        |begin
        |  rightTape := stringFromAsciiCode(currentCell) + rightTape;
        |
        |  if stringLength(leftTape) = 0 then
        |    currentCell := 0
        |  else
        |  begin
        |    lastIndex := stringLength(leftTape) - 1;
        |    currentCell := asciiCodeFromString(stringCharAt(leftTape, lastIndex));
        |    leftTape := stringLeftUntilIndex(leftTape, lastIndex);
        |  end;
        |end;
        |
        |begin
        |  leftTape := "";
        |  rightTape := "";
        |  currentCell := 65;
        |  MoveRight();
        |  currentCell := 66;
        |  MoveLeft();
        |  printLine(stringFromAsciiCode(currentCell));
        |end.
        |""".stripMargin
    )

    assertEquals(io.written, s"A$nl")
  }
}
