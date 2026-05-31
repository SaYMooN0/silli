package Interpreter

import scala.collection.mutable
import scala.io.StdIn

final class IOCtx private(
                           val readLine: () => String,
                           val writeLine: String => Unit
                         )

object IOCtx {
  def createForConsole(): IOCtx =
    new IOCtx(
      readLine = () => StdIn.readLine(),
      writeLine = text => println(text)
    )

  def createForTesting(inputLines: List[String]): (IOCtx, () => List[String]) = {
    val inputQueue = mutable.Queue.from(inputLines)
    val outputLines = mutable.ListBuffer.empty[String]

    val ctx =
      new IOCtx(
        readLine = () => {
          if (inputQueue.nonEmpty) inputQueue.dequeue()
          else throw RuntimeException("No more input lines available")

        },

        writeLine = text => {
          outputLines += text
        }
      )

    val getOutput = () => outputLines.toList

    (ctx, getOutput)
  }
}