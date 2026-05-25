package Interpreter

import Parser.Ident

final class RuntimeCtx private(
                                val io: IOCtx,
                                val callStack: CallStack
                              )

object RuntimeCtx {
  def init(programName: Ident, io: IOCtx) = new RuntimeCtx(io, CallStack.init(programName))
}

final case class InterpreterRuntime[+A](run: RuntimeCtx => Either[RuntimeErr, (A, RuntimeCtx)]) {
  def flatMap[B](binder: A => InterpreterRuntime[B]): InterpreterRuntime[B] =
    InterpreterRuntime { ctx =>
      run(ctx) match {
        case Right((value, nextCtx)) => binder(value).run(nextCtx)
        case Left(err) => Left(err)
      }
    }

  def map[B](mapper: A => B): InterpreterRuntime[B] =
    InterpreterRuntime { ctx =>
      run(ctx).map((value, nextCtx) => (mapper(value), nextCtx))
    }
}


object InterpreterRuntime {
  def pure[A](value: A): InterpreterRuntime[A] =
    InterpreterRuntime(ctx => Right((value, ctx)))

  def fail(err: RuntimeErr): InterpreterRuntime[Nothing] =
    InterpreterRuntime(_ => Left(err))

  //  def currentCtx: InterpreterRuntime[RuntimeCtx] =
  //    InterpreterRuntime(ctx => Right((ctx, ctx)))
  //
  //  def updateCtx(updater: RuntimeCtx => RuntimeCtx): InterpreterRuntime[Unit] =
  //    InterpreterRuntime { ctx =>
  //      Right(((), updater(ctx)))
  //    }
  //
  //  def currentCallStack: InterpreterRuntime[CallStack] =
  //    currentCtx.map(_.callStack)
  //
  //  def updateCallStack(updater: CallStack => CallStack): InterpreterRuntime[Unit] =
  //    updateCtx(ctx => ctx.copy(callStack = updater(ctx.callStack)))
  //
  //  def readLine: InterpreterRuntime[String] =
  //    currentCtx.map(_.io.readLine())
  //
  //  def writeLine(text: String): InterpreterRuntime[Unit] =
  //    currentCtx.map(_.io.writeLine(text))
}