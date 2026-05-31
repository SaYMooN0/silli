package Lexer

import java.io.Reader

final case class Pos(line: Int, column: Int) {
  require(line >= 1, "line must be >= 1")
  require(column >= 1, "column must be >= 1")

  def advanceInSameLine(): Pos = Pos(line = this.line, column = this.column + 1)

  def goToNextLine(): Pos = Pos(line = this.line + 1, column = 1)
}

final case class Loc(start: Pos, end: Pos) {
  require(end.line > start.line || end.column > start.column, "end must be after start")
}


final case class ReadingCtx(
                             current: ReaderChar,
                             next: ReaderChar,
                             reader: Reader,
                             currentPos: Pos
                           ) {
  def advanceInSameLine(): ReadingCtx =
    ReadingCtx(
      current = this.next,
      next = this.reader.readNextInputChar(),
      reader = this.reader,
      currentPos = currentPos.advanceInSameLine()
    )

  def advanceByInSameLine(count: Int): ReadingCtx = {
    require(count >= 0, "count must be >= 0")

    (0 until count).foldLeft(this) { (acc, _) =>
      acc.advanceInSameLine()
    }
  }

  def advanceToNewLine(): ReadingCtx =
    ReadingCtx(
      current = this.next,
      next = this.reader.readNextInputChar(),
      reader = this.reader,
      currentPos = currentPos.goToNextLine()
    )
}

object ReadingCtx {
  def init(reader: Reader): ReadingCtx = {
    val curCh = reader.readNextInputChar()
    val nxtCh = reader.readNextInputChar()
    ReadingCtx(curCh, nxtCh, reader, Pos(1, 1))
  }
}