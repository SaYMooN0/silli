
package Parser

import java.io.Reader
import _root_.Lexer.Lexer.{LexerReadNextFunc, LexerReadNextResult}
import _root_.Lexer.*

private[Parser] final class LexerWithState(
                                            private val lexerReadNextTokenFunc: LexerReadNextFunc,
                                            private val readingCtx: ReadingCtx
                                          ) {
  def readNextToken(): (LexerWithState, LexerReadNextResult) = {
    val (newCtx, readingRes) = lexerReadNextTokenFunc(this.readingCtx)
    (LexerWithState(lexerReadNextTokenFunc, newCtx), readingRes)

  }
}

private final case class ParsingCtx(
                                     curT: TokenWithLoc[?],
                                     nxtT: TokenWithLoc[?],
                                     private val lexer: LexerWithState
                                   ) {


  def advance: Either[ParserErr, ParsingCtx] =
    this.lexer.readNextToken() match {
      case (_, EndOfReaderReached) => Left(ParserErr.UnexpectedEndOfReader(this.nxtT))
      case (_, err: TokenizingErr) => Left(ParserErr.TokenizingFailed(this.curT.loc.end, err))
      case (lexerWithUpdatedState, newToken: TokenWithLoc[?]) =>
        Right(
          this.copy(
            curT = this.nxtT,
            nxtT = newToken,
            lexer = lexerWithUpdatedState
          )
        )
    }
}

object ParsingCtx {
  def init(
            inputReader: Reader,
            lexerReadNextTokenFunc: LexerReadNextFunc
          ): ParsingCtx | TokenizingErr | EndOfReaderReached.type
  = {
    lexerReadNextTokenFunc(ReadingCtx.init(inputReader)) match {
      case (_, e: TokenizingErr) => e
      case (_, EndOfReaderReached) => EndOfReaderReached
      case (ctxAfterFirst, firstToken: TokenWithLoc[?]) => lexerReadNextTokenFunc(ctxAfterFirst) match {
        case (_, e: TokenizingErr) => e
        case (_, EndOfReaderReached) => EndOfReaderReached
        case (ctxAfterSecond, secondToken: TokenWithLoc[?]) => ParsingCtx(
          firstToken, secondToken,
          LexerWithState(lexerReadNextTokenFunc, ctxAfterSecond)
        )
      }
    }
  }
}