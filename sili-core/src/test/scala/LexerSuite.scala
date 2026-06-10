import Lexer.*
import munit.FunSuite

import java.io.StringReader

final class LexerSuite extends FunSuite {

  private def readAll(input: String): Either[TokenizingErr, List[TokenWithLoc[?]]] = {
    val readNext = _root_.Lexer.Lexer.getDefaultReadNextTokenFunc

    def loop(ctx: ReadingCtx, gathered: List[TokenWithLoc[?]]): Either[TokenizingErr, List[TokenWithLoc[?]]] = {
      val (nextCtx, result) = readNext(ctx)
      result match {
        case EndOfReaderReached(_) => Right(gathered.reverse)
        case err: TokenizingErr    => Left(err)
        case token: TokenWithLoc[?] => loop(nextCtx, token :: gathered)
      }
    }

    loop(ReadingCtx.init(new StringReader(input)), List.empty)
  }

  private def tokensOf(input: String): List[Token] = {
    readAll(input) match {
      case Right(tokens) => tokens.map(_.token)
      case Left(err)    => fail(s"Tokenizing failed: $err")
    }
  }

  private def tokenLocsOf(input: String): List[Loc] = {
    readAll(input) match {
      case Right(tokens) => tokens.map(_.loc)
      case Left(err)    => fail(s"Tokenizing failed: $err")
    }
  }

  private def assertTokenizingFails(input: String, expectedPart: String): Unit = {
    readAll(input) match {
      case Left(err) => assert(err.toString.contains(expectedPart), s"Expected '$expectedPart' inside '$err'")
      case Right(tokens) => fail(s"Expected tokenizing error, but got tokens: ${tokens.map(_.token)}")
    }
  }

  test("recognizes program skeleton keywords and punctuation") {
    val tokens = tokensOf("program Main; begin end.")

    assertEquals(
      tokens,
      List(
        SyntaxKeywordToken.Program,
        IdentToken("Main"),
        SimpleToken.SemiColon,
        SyntaxKeywordToken.Begin,
        SyntaxKeywordToken.End,
        SimpleToken.Dot
      )
    )
  }

  test("recognizes declaration keywords and built-in type names") {
    val tokens = tokensOf("var a : integer; b : real; c : boolean; d : string;")

    assertEquals(
      tokens,
      List(
        DeclarationKeywordToken.Var,
        IdentToken("a"),
        SimpleToken.Colon,
        BuiltInTypeNameToken(TypeSystem.BuiltInType.IntegerT),
        SimpleToken.SemiColon,
        IdentToken("b"),
        SimpleToken.Colon,
        BuiltInTypeNameToken(TypeSystem.BuiltInType.RealT),
        SimpleToken.SemiColon,
        IdentToken("c"),
        SimpleToken.Colon,
        BuiltInTypeNameToken(TypeSystem.BuiltInType.BooleanT),
        SimpleToken.SemiColon,
        IdentToken("d"),
        SimpleToken.Colon,
        BuiltInTypeNameToken(TypeSystem.BuiltInType.StringT),
        SimpleToken.SemiColon
      )
    )
  }

  test("recognizes procedure, function, if, then, else keywords") {
    val tokens = tokensOf("procedure P; function F : integer; if true then else")

    assertEquals(
      tokens,
      List(
        DeclarationKeywordToken.Procedure,
        IdentToken("P"),
        SimpleToken.SemiColon,
        DeclarationKeywordToken.Function,
        IdentToken("F"),
        SimpleToken.Colon,
        BuiltInTypeNameToken(TypeSystem.BuiltInType.IntegerT),
        SimpleToken.SemiColon,
        SyntaxKeywordToken.If,
        BooleanLiteralToken.True,
        SyntaxKeywordToken.Then,
        SyntaxKeywordToken.Else
      )
    )
  }

  test("recognizes boolean literals separately from identifiers") {
    val tokens = tokensOf("true false trueValue falseValue")

    assertEquals(
      tokens,
      List(
        BooleanLiteralToken.True,
        BooleanLiteralToken.False,
        IdentToken("trueValue"),
        IdentToken("falseValue")
      )
    )
  }

  test("recognizes arithmetic, comparison and logical operators") {
    val tokens = tokensOf("+ - * / div mod = <> < <= > >= and or xor not")

    assertEquals(
      tokens,
      List(
        OpToken.Plus,
        OpToken.Minus,
        OpToken.Mul,
        OpToken.RealDiv,
        OpToken.Div,
        OpToken.Mod,
        OpToken.Equal,
        OpToken.NotEqual,
        OpToken.Less,
        OpToken.LessOrEqual,
        OpToken.Greater,
        OpToken.GreaterOrEqual,
        OpToken.And,
        OpToken.Or,
        OpToken.Xor,
        OpToken.Not
      )
    )
  }

  test("recognizes assignment before colon") {
    val tokens = tokensOf("x := 10 : integer")

    assertEquals(
      tokens,
      List(
        IdentToken("x"),
        SimpleToken.Assign,
        IntegerNumLiteralToken(10),
        SimpleToken.Colon,
        BuiltInTypeNameToken(TypeSystem.BuiltInType.IntegerT)
      )
    )
  }

  test("recognizes commas and parentheses in call syntax") {
    val tokens = tokensOf("Alpha(1, 2, 3)")

    assertEquals(
      tokens,
      List(
        IdentToken("Alpha"),
        SimpleToken.LPar,
        IntegerNumLiteralToken(1),
        SimpleToken.Comma,
        IntegerNumLiteralToken(2),
        SimpleToken.Comma,
        IntegerNumLiteralToken(3),
        SimpleToken.RPar
      )
    )
  }

  test("recognizes integer and real numeric literals") {
    val tokens = tokensOf("0 1 42 3.14 10.0")

    assertEquals(
      tokens,
      List(
        IntegerNumLiteralToken(0),
        IntegerNumLiteralToken(1),
        IntegerNumLiteralToken(42),
        RealNumLiteralToken(3.14),
        RealNumLiteralToken(10.0)
      )
    )
  }

  test("recognizes string literals") {
    val tokens = tokensOf("\"hello\" \"world\"")

    assertEquals(
      tokens,
      List(
        StringLiteralToken("hello"),
        StringLiteralToken("world")
      )
    )
  }

  test("recognizes escaped string literal content") {
    val tokens = tokensOf("\"a\\nb\" \"a\\tb\" \"a\\\"b\" \"a\\\\b\"")

    assertEquals(
      tokens,
      List(
        StringLiteralToken("a\nb"),
        StringLiteralToken("a\tb"),
        StringLiteralToken("a\"b"),
        StringLiteralToken("a\\b")
      )
    )
  }

  test("skips simple comments") {
    val tokens = tokensOf("x { this is a comment } y")

    assertEquals(tokens, List(IdentToken("x"), IdentToken("y")))
  }

  test("skips comments containing operators and keywords") {
    val tokens = tokensOf("x { begin + - * := } y")

    assertEquals(tokens, List(IdentToken("x"), IdentToken("y")))
  }

  test("skips multiline comments and keeps reading after them") {
    val tokens = tokensOf("x { line 1\nline 2\nline 3 } y")

    assertEquals(tokens, List(IdentToken("x"), IdentToken("y")))
  }

  test("keeps token locations on a single line") {
    val tokens = readAll("abc := 123;").getOrElse(fail("tokenizing failed"))

    assertEquals(tokens(0).loc, Loc(Pos(1, 1), Pos(1, 4)))
    assertEquals(tokens(1).loc, Loc(Pos(1, 5), Pos(1, 7)))
    assertEquals(tokens(2).loc, Loc(Pos(1, 8), Pos(1, 11)))
    assertEquals(tokens(3).loc, Loc(Pos(1, 11), Pos(1, 12)))
  }

  test("keeps token locations after line feed") {
    val tokens = readAll("program Main;\nbegin\nend.").getOrElse(fail("tokenizing failed"))

    assertEquals(tokens.map(_.loc.start), List(
      Pos(1, 1),
      Pos(1, 9),
      Pos(1, 13),
      Pos(2, 1),
      Pos(3, 1),
      Pos(3, 4)
    ))
  }

  test("keeps token locations after CRLF") {
    val locs = tokenLocsOf("x\r\ny")

    assertEquals(locs.map(_.start), List(Pos(1, 1), Pos(2, 1)))
  }

  test("reports unexpected character") {
    assertTokenizingFails("x @ y", "UnexpectedChar")
  }

  test("reports unclosed comment") {
    assertTokenizingFails("x { comment", "UnclosedComment")
  }

  test("reports unclosed string literal on end of file") {
    assertTokenizingFails("\"hello", "StringLiteralErr")
  }

  test("reports unclosed string literal on newline") {
    assertTokenizingFails("\"hello\nworld\"", "StringLiteralErr")
  }

  test("reports invalid escape sequence in string literal") {
    assertTokenizingFails("\"a\\qb\"", "StringLiteralErr")
  }

  test("reports integer literal that does not fit Int") {
    assertTokenizingFails("999999999999999999999999999999", "NumLiteralErr")
  }

  test("handles full small program token stream") {
    val tokens = tokensOf(
      """
        |program Main;
        |var x : integer;
        |begin
        |  x := (1 + 2) * 3;
        |end.
        |""".stripMargin
    )

    assertEquals(tokens.head, SyntaxKeywordToken.Program)
    assertEquals(tokens.last, SimpleToken.Dot)
    assert(tokens.contains(SimpleToken.Assign))
    assert(tokens.contains(OpToken.Plus))
    assert(tokens.contains(OpToken.Mul))
  }

  test("does not treat keyword prefixes as keywords") {
    val tokens = tokensOf("beginner endValue integerValue trueValue")

    assertEquals(
      tokens,
      List(
        IdentToken("beginner"),
        IdentToken("endValue"),
        IdentToken("integerValue"),
        IdentToken("trueValue")
      )
    )
  }

  test("recognizes stdlib names as normal identifiers") {
    val tokens = tokensOf("printLine readLine stringLength convertIntegerToString")

    assertEquals(
      tokens,
      List(
        IdentToken("printLine"),
        IdentToken("readLine"),
        IdentToken("stringLength"),
        IdentToken("convertIntegerToString")
      )
    )
  }

  test("recognizes empty string literal") {
    assertEquals(tokensOf("\"\""), List(StringLiteralToken("")))
  }

  test("recognizes mod as operator keyword") {
    assertEquals(tokensOf("a mod b"), List(IdentToken("a"), OpToken.Mod, IdentToken("b")))
  }
}
