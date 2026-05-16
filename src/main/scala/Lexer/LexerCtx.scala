package Lexer

import java.io.Reader

final case class Pos(line: Int, column: Int) {
  require(line >= 1, "line must be >= 1")
  require(column >= 1, "column must be >= 1")
}

final case class Loc(start: Pos, end: Pos) {
  require(end.line > start.line || end.column > start.column, "end must be after start")
}


private[Lexer] final case class LexerCtx(
                                          current: ReaderChar,
                                          next: ReaderChar,
                                          reader: Reader
                                        ) {
  def advanceBy1(): LexerCtx =
    LexerCtx(
      current = this.next,
      next = this.reader.readNextInputChar(),
      reader = this.reader
    )
}