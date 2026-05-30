package Interpreter

import Lexer.Loc
import Parser.Ident

final class RuntimeCtx private(
                                val io: IOCtx,
                                val callstack: Callstack
                              )

object RuntimeCtx {
  def init(programName: Ident, io: IOCtx) = new RuntimeCtx(io, Callstack.init(programName))
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

  def failWIthInternalErr(loc: Loc, message: String): InterpreterRuntime[Nothing] =
    InterpreterRuntime(_ => Left(RuntimeErr.InternalInterpreterErr(loc, message)))

  private def currentCtx: InterpreterRuntime[RuntimeCtx] =
    InterpreterRuntime(ctx => Right((ctx, ctx)))
  def callstack: InterpreterRuntime[Callstack] =
    currentCtx.map(_.callstack)
}