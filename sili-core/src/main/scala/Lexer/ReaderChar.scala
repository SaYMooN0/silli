package Lexer

import java.io.Reader

case object EOF // end of file


type ReaderChar = Char | EOF.type;

extension (reader: Reader) {
  private[Lexer] def readNextInputChar(): ReaderChar = {
    val code = reader.read()

    if code == -1 then EOF
    else code.toChar
  }
}