package Lexer


enum NumLiteralReaderErr(val pos: Pos) {
  case UnableToParse(stringValue: String, loc: Loc) extends NumLiteralReaderErr(loc.start)
}

private[Lexer] object NumLiteralReader {
  private case class NumLiteralReadingCtx(capRev: List[Char], literalStartPos: Pos, readingCtx: ReadingCtx)

  private type NumLiteral = IntegerNumLiteralToken | RealNumLiteralToken;

  def readFromFirstDigit(firstDigit: Char, ctx: ReadingCtx): (
    ReadingCtx, Either[NumLiteralReaderErr, TokenWithLoc[NumLiteral]])
  = {
    if (!firstDigit.isDigit) throw new Error(s"'$firstDigit' passed to the NumLiteralReader must be a digit")
    val literalReadingCtx = NumLiteralReadingCtx(List(firstDigit), ctx.currentPos, ctx.advanceInSameLine());
    continueNumLiteralTillOut(literalReadingCtx)
  }

  private def continueNumLiteralTillOut(ctx: NumLiteralReadingCtx):
  (ReadingCtx, Either[NumLiteralReaderErr, TokenWithLoc[NumLiteral]])
  = {
    ctx.readingCtx.current match {
      case ch: Char if ch.isDigit => continueNumLiteralTillOut(ctx.copy(
        capRev = ch :: ctx.capRev,
        readingCtx = ctx.readingCtx.advanceInSameLine()
      ))
      case '.' if !ctx.capRev.contains('.') => continueNumLiteralTillOut(ctx.copy(
        capRev = '.' :: ctx.capRev,
        readingCtx = ctx.readingCtx.advanceInSameLine()
      ))
      case otherCh => constructLiteralOnOut(ctx)

    }
  }

  private def constructLiteralOnOut(ctx: NumLiteralReadingCtx):
  (ReadingCtx, Either[NumLiteralReaderErr, TokenWithLoc[NumLiteral]])
  = {

    val numInStr = ctx.capRev.reverse.mkString
    val litLoc = Loc(start = ctx.literalStartPos, end = ctx.readingCtx.currentPos)

    if (numInStr.contains('.')) {
      numInStr.toDoubleOption match {
        case Some(value) => (ctx.readingCtx, Right(TokenWithLoc(RealNumLiteralToken(value), litLoc)))
        case None => (ctx.readingCtx, Left(NumLiteralReaderErr.UnableToParse(numInStr, litLoc)))
      }
    } else {
      numInStr.toIntOption match {
        case Some(value) => (ctx.readingCtx, Right(TokenWithLoc(IntegerNumLiteralToken(value), litLoc)))
        case None => (ctx.readingCtx, Left(NumLiteralReaderErr.UnableToParse(numInStr, litLoc)))
      }
    }
  }
}

