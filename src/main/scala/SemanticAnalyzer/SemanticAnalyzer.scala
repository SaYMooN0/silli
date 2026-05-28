package SemanticAnalyzer

import Parser.Ident


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

  def currentScope: SemanticAnalyzer[GlobalScopeSymbolTable | ScopedSymbolTable] =
    SemanticAnalyzer(ctx => (ctx.currentScope, ctx))

  private def updateCtx(updater: SemanticCtx => SemanticCtx): SemanticAnalyzer[Unit] =
    SemanticAnalyzer(ctx => ((), updater(ctx)))


  def reportErr(err: SemanticErr): SemanticAnalyzer[Unit] =
    updateCtx { ctx => ctx.copy(errors = ctx.errors :+ err) }

  def reportErrAndMapNone[A](err: SemanticErr): SemanticAnalyzer[Option[A]] =
    reportErr(err).map(_ => None)

  def addSymbolToCurrentScope(name: Ident, symbol: SemanticSymbol): SemanticAnalyzer[Unit] =
    updateCtx { ctx =>
      val updatedScope: GlobalScopeSymbolTable | ScopedSymbolTable =
        ctx.currentScope match {
          case global: GlobalScopeSymbolTable => global.withSymbol(name, symbol)
          case scoped: ScopedSymbolTable => scoped.withSymbol(name, symbol)
        }
      ctx.copy(currentScope = updatedScope)
    }
}

final case class SemanticCtx(
                              errors: Vector[SemanticErr],
                              currentScope: GlobalScopeSymbolTable | ScopedSymbolTable
                              // scopes, symbols, current procedure, etc.
                            )

object SemanticCtx {
  def init: SemanticCtx = SemanticCtx(
    errors = Vector.empty,
    currentScope = GlobalScopeSymbolTable.init
  )
}