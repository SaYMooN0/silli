package SemanticAnalyzer


final case class SemanticAnalyzer[+A](run: SemanticCtx => (A, SemanticCtx)) {
  def flatMap[B](binder: A => SemanticAnalyzer[B]): SemanticAnalyzer[B] =
    SemanticAnalyzer { ctx =>
      val (value, nextCtx) = run(ctx)
      binder(value).run(nextCtx)
    }

  def map[B](mapper: A => B): SemanticAnalyzer[B] =
    SemanticAnalyzer { ctx =>
      val (value, nextCtx) = run(ctx)
      (mapper(value), nextCtx)
    }
}

object SemanticAnalyzer {
  def pure[A](value: A): SemanticAnalyzer[A] =
    SemanticAnalyzer(ctx => (value, ctx))

  //  def getCtx: SemanticAnalyzer[SemanticCtx] =
  //    SemanticAnalyzer(ctx => (ctx, ctx))

  private def updateCtx(updater: SemanticCtx => SemanticCtx): SemanticAnalyzer[Unit] =
    SemanticAnalyzer(ctx => ((), updater(ctx)))

  private def reportErr(err: SemanticErr): SemanticAnalyzer[Unit] =
    updateCtx(ctx => ctx.copy(errors = ctx.errors :+ err))

  def reportErrAndMapNone[A](err: SemanticErr): SemanticAnalyzer[Option[A]] =
    updateCtx(ctx => ctx.copy(errors = ctx.errors :+ err)).map(_ => None)
}

final case class SemanticCtx(
                              errors: Vector[SemanticErr],
                              // scopes, symbols, current procedure, etc.
                            )

object SemanticCtx {
  def init: SemanticCtx =
    SemanticCtx(errors = Vector.empty)
}