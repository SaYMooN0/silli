package Lexer

enum StringLiteralReaderErr {
  case InvalidEsqSeqInStringLiteral(char: ReaderChar, backslashPos: Pos)
  case UnclosedStringLiteral(openPos: Pos, expectedToBeClosed: Pos)
}

object StringLiteralReader {
  private case class StringLiteralReadingCtx(capRev: List[Char], openQuotePos: Pos, readingCtx: ReadingCtx)

  def readFromQuote(ctx: ReadingCtx): (ReadingCtx, Either[StringLiteralReaderErr, TokenWithLoc[StringLiteralToken]]) = {
    val stringLiteralReadingCtx = StringLiteralReadingCtx(List(), ctx.currentPos, ctx.advanceInSameLine());
    continueLiteralTillOut(stringLiteralReadingCtx)
  }

  private def continueLiteralTillOut(ctx: StringLiteralReadingCtx):
  (ReadingCtx, Either[StringLiteralReaderErr, TokenWithLoc[StringLiteralToken]])
  = {
    (ctx.readingCtx.current, ctx.readingCtx.next) match {
      case (EOF, _) => (ctx.readingCtx, Left(StringLiteralReaderErr.UnclosedStringLiteral(ctx.openQuotePos, ctx.readingCtx.currentPos)))
      case ('\n', _) => (ctx.readingCtx, Left(StringLiteralReaderErr.UnclosedStringLiteral(ctx.openQuotePos, ctx.readingCtx.currentPos)))
      case ('"', _) => {
        val litVal = StringLiteralToken(ctx.capRev.reverse.mkString)

        val readingCtxOnOut = ctx.readingCtx.advanceInSameLine()
        val loc = Loc(start = ctx.openQuotePos, end = readingCtxOnOut.currentPos)
        (readingCtxOnOut, Right(TokenWithLoc(litVal, loc)))
      }
      case ('\\', '\\') => addToCapAndAdvanceBy2(ctx, '\\')
      case ('\\', '"') => addToCapAndAdvanceBy2(ctx, '"')
      case ('\\', 'n') => addToCapAndAdvanceBy2(ctx, '\n')
      case ('\\', 't') => addToCapAndAdvanceBy2(ctx, '\t')
      case ('\\', unexpectedChar) => (ctx.readingCtx, Left(
        StringLiteralReaderErr.InvalidEsqSeqInStringLiteral(unexpectedChar, ctx.readingCtx.currentPos)
      ))
      case (ch: Char, _) => continueLiteralTillOut(ctx.copy(
        capRev = ch :: ctx.capRev,
        readingCtx = ctx.readingCtx.advanceInSameLine()
      ))
    }
  }

  private def addToCapAndAdvanceBy2(ctx: StringLiteralReadingCtx, ch: Char) = continueLiteralTillOut(ctx.copy(
    capRev = ch :: ctx.capRev,
    readingCtx = ctx.readingCtx.advanceInSameLine().advanceToNewLine()
  ))
}
