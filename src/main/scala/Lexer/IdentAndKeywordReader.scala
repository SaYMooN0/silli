package Lexer

object IdentAndKeywordReader {
  def isCharCorrectIdentStarter(ch: Char) = ch.isLetter || ch == '_'

  private def canBeInIdentifier(ch: Char) = ch.isDigit || isCharCorrectIdentStarter(ch)

  private case class IdentAndKeywordReadingCtx(capRev: List[Char], startPos: Pos, readingCtx: ReadingCtx)

  private type IdentOrKeyword = IdentToken | Keyword;

  def readFromFirstChar(firsChar: Char, ctx: ReadingCtx):
  (ReadingCtx, TokenWithLoc[IdentOrKeyword])
  = {
    if (!isCharCorrectIdentStarter(firsChar)) throw new Error(s"'$firsChar' passed to the IdentAndKeywordReader must be a correct ident starter")
    val literalReadingCtx = IdentAndKeywordReadingCtx(List(firsChar), ctx.currentPos, ctx.advanceInSameLine());
    continueTillOut(literalReadingCtx)
  }

  private def continueTillOut(ctx: IdentAndKeywordReadingCtx):
  (ReadingCtx, TokenWithLoc[IdentOrKeyword])
  = {
    ctx.readingCtx.current match {
      case ch: Char if canBeInIdentifier(ch) => continueTillOut(ctx.copy(
        capRev = ch :: ctx.capRev,
        readingCtx = ctx.readingCtx.advanceInSameLine()
      ))
      case otherCh => constructOnOut(ctx)

    }
  }

  private def constructOnOut(ctx: IdentAndKeywordReadingCtx):
  (ReadingCtx, TokenWithLoc[IdentOrKeyword])
  = {
    val str = ctx.capRev.reverse.mkString
    val withLoc = (t: IdentOrKeyword) => TokenWithLoc(t, loc = Loc(ctx.startPos, ctx.readingCtx.currentPos))
    mapToKeyword(str) match {
      case Some(kw) => (ctx.readingCtx, withLoc(kw))
      case None => (ctx.readingCtx, withLoc(IdentToken(str)))
    }
  }
}
