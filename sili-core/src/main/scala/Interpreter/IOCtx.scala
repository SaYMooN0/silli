package Interpreter

trait IOCtx {
  def readLine(): String

  /**
   * Returns ASCII/byte code of one input character.
   * Returns -1 on end of input.
   */
  def readSingleCharAsAsciiCode(): Int

  def write(value: String): Unit

  def writeLine(value: String): Unit =
    write(value + System.lineSeparator())
}
