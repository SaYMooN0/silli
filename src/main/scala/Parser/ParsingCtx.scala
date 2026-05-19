
package Parser

import java.io.Reader
import _root_.Lexer.Lexer.LexerReadNextResult
import _root_.Lexer.{EndOfReaderReached, Loc, ReadingCtx, Token, TokenWithLoc, TokenizingErr}

private[Parser] final class LexerWithState(
                                            private val lexerReadNextTokenFunc: ReadingCtx => (ReadingCtx, LexerReadNextResult),
                                            private val readingCtx: ReadingCtx
                                          ) {
  def readNextToken(): (LexerWithState, LexerReadNextResult) = {
    val (newCtx, readingRes) = lexerReadNextTokenFunc(this.readingCtx)
    (LexerWithState(lexerReadNextTokenFunc, newCtx), readingRes)

  }
}

private final case class ParsingCtx(
                                     curToken: Token,
                                     curTokenLoc: Loc,
                                     nxtT: Token,
                                     nxtTokenLoc: Loc,
                                     private val lexer: LexerWithState
                                   ) {


  def advance: ParsingCtx | TokenizingErr | EndOfReaderReached.type = {
    this.lexer.readNextToken() match {
      case (_, EndOfReaderReached) => EndOfReaderReached
      case (_, err: TokenizingErr) => err
      case (lexerWithUpdatedState, newToken: TokenWithLoc[_]) => this.copy(
        curToken = this.nxtT,
        curTokenLoc = this.nxtTokenLoc,
        nxtT = newToken.token,
        nxtTokenLoc = newToken.loc,
        lexer = lexerWithUpdatedState
      )
    }
  }
}

object ParsingCtx {
  def init(
            inputReader: Reader,
            lexerReadNextTokenFunc: ReadingCtx => (ReadingCtx, LexerReadNextResult)
          ): ParsingCtx | TokenizingErr | EndOfReaderReached.type
  = {
    lexerReadNextTokenFunc(ReadingCtx.init(inputReader)) match {
      case (_, e: TokenizingErr) => e
      case (_, EndOfReaderReached) => EndOfReaderReached
      case (ctxAfterFirst, firstToken: TokenWithLoc[_]) => lexerReadNextTokenFunc(ctxAfterFirst) match {
        case (_, e: TokenizingErr) => e
        case (_, EndOfReaderReached) => EndOfReaderReached
        case (ctxAfterSecond, secondToken: TokenWithLoc[_]) => ParsingCtx(
          firstToken.token, firstToken.loc,
          secondToken.token, secondToken.loc,
          LexerWithState(lexerReadNextTokenFunc, ctxAfterSecond)
        )
      }
    }
  }
}