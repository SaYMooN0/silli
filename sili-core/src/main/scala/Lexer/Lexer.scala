package Lexer

import java.io.StringReader


enum TokenizingErr(val pos: Pos) {
  case UnexpectedCharErr(char: Char, charPos: Pos, nextChar: ReaderChar) extends TokenizingErr(charPos)
  case UnclosedComment(openPos: Pos) extends TokenizingErr(openPos)
  case StringLiteralErr(err: StringLiteralReaderErr) extends TokenizingErr(err.pos)
  case NumLiteralErr(err: NumLiteralReaderErr) extends TokenizingErr(err.pos)
}

final case class EndOfReaderReached(endOfReaderPos: Pos)

object Lexer {

  def getDefaultReadNextTokenFunc: LexerReadNextFunc = {
    readNext
  }

  type LexerReadNextResult = TokenizingErr | TokenWithLoc[?] | EndOfReaderReached
  type LexerReadNextFunc = ReadingCtx => (ReadingCtx, LexerReadNextResult)

  private val readNext: LexerReadNextFunc = (ctx: ReadingCtx) => {
    (ctx.current, ctx.next) match {

      case (EOF, _) => (ctx, EndOfReaderReached(ctx.currentPos))
      case ('\n', _) => readNext(ctx.advanceToNewLine())
      case ('\r', '\n') => readNext(ctx.advanceInSameLine().advanceToNewLine())
      case (' ', _) => readNext(ctx.advanceInSameLine())

      case (':', '=') => advanceInSameLine(ctx, SimpleToken.Assign, 2)
      case ('<', '>') => advanceInSameLine(ctx, OpToken.NotEqual, 2)
      case ('<', '=') => advanceInSameLine(ctx, OpToken.LessOrEqual, 2)
      case ('>', '=') => advanceInSameLine(ctx, OpToken.GreaterOrEqual, 2)


      case (':', _) => advanceInSameLine(ctx, SimpleToken.Colon, 1)
      case (';', _) => advanceInSameLine(ctx, SimpleToken.SemiColon, 1)
      case ('.', _) => advanceInSameLine(ctx, SimpleToken.Dot, 1)
      case (',', _) => advanceInSameLine(ctx, SimpleToken.Comma, 1)
      case ('(', _) => advanceInSameLine(ctx, SimpleToken.LPar, 1)
      case (')', _) => advanceInSameLine(ctx, SimpleToken.RPar, 1)
      case ('+', _) => advanceInSameLine(ctx, OpToken.Plus, 1)
      case ('-', _) => advanceInSameLine(ctx, OpToken.Minus, 1)
      case ('*', _) => advanceInSameLine(ctx, OpToken.Mul, 1)
      case ('/', _) => advanceInSameLine(ctx, OpToken.RealDiv, 1)
      case ('=', _) => advanceInSameLine(ctx, OpToken.Equal, 1)
      case ('<', _) => advanceInSameLine(ctx, OpToken.Less, 1)
      case ('>', _) => advanceInSameLine(ctx, OpToken.Greater, 1)
      case ('{', _) => skipTillRightCurly(ctx.currentPos, ctx.advanceInSameLine()) match {
        case Left(err, newCtx) => (newCtx, err)
        case Right(newCtx) => readNext(newCtx)
      }

      case ('"', _) => StringLiteralReader.readFromQuote(ctx) match {
        case (newCtx, Left(err)) => (newCtx, TokenizingErr.StringLiteralErr(err))
        case (newCtx, Right(lit)) => (newCtx, lit)
      }

      case (a: Char, b) if (a.isDigit) => NumLiteralReader.readFromFirstDigit(a, ctx) match {
        case (newCtx, Left(err)) => (newCtx, TokenizingErr.NumLiteralErr(err))
        case (newCtx, Right(token)) => (newCtx, token)
      }
      case (a: Char, b) if IdentRules.isCorrectIdentStarter(a) => IdentAndKeywordReader.readFromFirstChar(a, ctx)
      case (a: Char, b) => (ctx, TokenizingErr.UnexpectedCharErr(a, ctx.currentPos, b))
    }
  }

  private def advanceInSameLine(ctx: ReadingCtx, token: Token, tokenLen: Int): (ReadingCtx, TokenWithLoc[?]) = {
    val tokenStartPos = ctx.currentPos
    val newCtx = ctx.advanceByInSameLine(tokenLen)
    val loc = Loc(tokenStartPos, newCtx.currentPos)

    (newCtx, TokenWithLoc(token, loc))
  }

  private def skipTillRightCurly(openingCurlyPos: Pos, ctx: ReadingCtx): Either[
    (TokenizingErr.UnclosedComment, ReadingCtx),
    ReadingCtx
  ] = {
    ctx.current match {
      case EOF => Left(TokenizingErr.UnclosedComment(openingCurlyPos), ctx)
      case '}' => Right(ctx.advanceInSameLine())
      case '\n' => skipTillRightCurly(openingCurlyPos, ctx.advanceToNewLine())
      case _ => skipTillRightCurly(openingCurlyPos, ctx.advanceInSameLine())
    }
  }

  def printAllTokens(str: String): Unit = {
    val startCtx = ReadingCtx.init(new StringReader(str))

    def printTokenAndContinueIfNotEnd(ctx: ReadingCtx): Unit = {
      val (newCtx, tokenRes) = readNext(ctx)
      println(tokenRes)
      tokenRes match {
        case e: EndOfReaderReached => ()
        case e: TokenizingErr =>
          throw new Error(s"$e")
        case _ => printTokenAndContinueIfNotEnd(newCtx)
      }
    }

    printTokenAndContinueIfNotEnd(startCtx)
  }
}
