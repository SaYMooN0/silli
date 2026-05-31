
package Parser

import _root_.Lexer.*
import _root_.Lexer.Lexer.{LexerReadNextFunc, LexerReadNextResult}

import java.io.Reader

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
                                     nxtT: TokenWithLoc[?] | EndOfReaderReached,
                                     private val lexer: LexerWithState
                                   ) {


  def advance: Either[ParserErr, ParsingCtx] = {
    (this.lexer.readNextToken(), this.nxtT) match {
      case ((_, err: TokenizingErr), _) => Left(ParserErr.TokenizingFailed(err))
      case (_, previousNxt: EndOfReaderReached) => Left(ParserErr.UnexpectedEndOfReader(previousNxt))
      case ((lexerWithUpdatedState, newNxt: (TokenWithLoc[?] | EndOfReaderReached)), previousNxt: TokenWithLoc[?]) =>
        Right(this.copy(
          curT = previousNxt,
          nxtT = newNxt,
          lexer = lexerWithUpdatedState
        ))
    }
  }
}

object ParsingCtx {
  def init(inputReader: Reader, lexerReadNextTokenFunc: LexerReadNextFunc): Either[ParserErr, ParsingCtx] = {
    val readingCtx = ReadingCtx.init(inputReader)
    lexerReadNextTokenFunc(readingCtx) match {
      case (_, e: TokenizingErr) => Left(ParserErr.TokenizingFailed(e))
      case (ctxAfterFirst, end: EndOfReaderReached) => Left(ParserErr.UnexpectedEndOfReader(end))

      case (ctxAfterFirst, firstToken: TokenWithLoc[?]) => lexerReadNextTokenFunc(ctxAfterFirst) match {
        case (_, e: TokenizingErr) => Left(ParserErr.TokenizingFailed(e))
        case (ctxAfterSecond, end: EndOfReaderReached) => Left(ParserErr.UnexpectedEndOfReader(end))
        case (ctxAfterSecond, secondToken: TokenWithLoc[?]) =>
          Right(ParsingCtx(firstToken, secondToken, LexerWithState(lexerReadNextTokenFunc, ctxAfterSecond)))
      }
    }
  }
}