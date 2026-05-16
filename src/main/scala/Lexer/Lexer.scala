package Lexer

import java.io.{Reader, StringReader}

class Lexer(reader: Reader) {
  private val ctx: LexerCtx =
    LexerCtx(reader.readNextInputChar(), reader.readNextInputChar(), reader)
  private var currentPos: Pos = Pos(1, 1);

  private def advanceInSameLine(token: Token, tokenLen: Int): TokenWithLoc = {
    val tokenStartP = this.currentPos;
    val tokenEndP = Pos(this.currentPos.line, this.currentPos.column + tokenLen);
    val loc = Loc(tokenStartP, tokenEndP)

    this.currentPos = tokenEndP;
    (token, loc)
  }

  private def advanceWith1CharToken(token: Token): TokenWithLoc =
    advanceInSameLine(token, 1)


  def readNext(): TokenWithLoc | UnexpectedCharErr | EndOfReaderReached.type =
    (ctx.current, ctx.next) match {
      case (EOF, _) => EndOfReaderReached
      case (':', '=') => advanceInSameLine(SimpleToken.Assign, 2)
      case (':', _) => advanceInSameLine(SimpleToken.Colon, 1)
      case (a: Char, b) => UnexpectedCharErr(a, b)
    }
}

object Lexer {
  def lexerForString(str: String): Lexer = {
    val reader = new StringReader(str)
    new Lexer(reader)
  }
}

class UnexpectedCharErr(char: Char, next: ReaderChar);

object EndOfReaderReached;
