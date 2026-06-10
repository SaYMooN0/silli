package testutils

import Interpreter.IOCtx

import scala.collection.mutable

/**
 * Test implementation of IOCtx.
 *
 * It is intentionally placed in test sources, not near the production IOCtx trait.
 * The interpreter core only depends on IOCtx, while tests can use this class to
 * provide deterministic input and inspect output without touching the console.
 */
final class TestIOCtx(
  lineInputs: List[String] = List.empty,
  charInputs: List[Int] = List.empty
) extends IOCtx {

  private val lineQueue: mutable.Queue[String] = mutable.Queue.from(lineInputs)
  private val charQueue: mutable.Queue[Int] = mutable.Queue.from(charInputs)
  private val out: StringBuilder = new StringBuilder

  override def readLine(): String = {
    if (lineQueue.nonEmpty) lineQueue.dequeue()
    else throw RuntimeException("No more input lines available in TestIOCtx")
  }

  override def readSingleCharAsAsciiCode(): Int = {
    if (charQueue.nonEmpty) charQueue.dequeue()
    else -1
  }

  override def write(value: String): Unit = {
    out.append(value)
    ()
  }

  def written: String = out.toString()

  def writtenLines: List[String] = {
    val text = written
    if (text.isEmpty) List.empty
    else text.linesIterator.toList
  }

  def clearOutput(): Unit = {
    out.clear()
    ()
  }

  def remainingLineInputsCount: Int = lineQueue.size

  def remainingCharInputsCount: Int = charQueue.size
}

object TestIOCtx {
  def empty: TestIOCtx = new TestIOCtx()

  def withLines(lines: String*): TestIOCtx =
    new TestIOCtx(lineInputs = lines.toList)

  def withCharCodes(codes: Int*): TestIOCtx =
    new TestIOCtx(charInputs = codes.toList)

  def withLinesAndCharCodes(lines: List[String], codes: List[Int]): TestIOCtx =
    new TestIOCtx(lineInputs = lines, charInputs = codes)
}
