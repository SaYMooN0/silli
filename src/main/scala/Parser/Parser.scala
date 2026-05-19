package Parser

import _root_.Lexer.*
import _root_.Lexer.Lexer.LexerReadNextResult


final case class Parser[+A](run: ParsingCtx => Either[ParserErr, (A, ParsingCtx)]) {
  def flatMap[B](binder: A => Parser[B]): Parser[B] =
    Parser { ctx =>
      run(ctx) match {
        case Right((value, nextCtx)) => binder(value).run(nextCtx)
        case Left(err) => Left(err)
      }
    }

  def map[B](mapper: A => B): Parser[B] =
    Parser { ctx =>
      run(ctx).map {
        case (value, nextCtx) => (mapper(value), nextCtx)
      }
    }
}

object Parser {
  def succeed[A](value: A): Parser[A] = Parser(ctx => Right((value, ctx)))

  val cur: Parser[TokenWithLoc[?]] = Parser(ctx => Right((ctx.curT, ctx)))

  val nxt: Parser[TokenWithLoc[?]] = Parser(ctx => Right((ctx.nxtT, ctx)))
 
  val curAndNxt: Parser[(TokenWithLoc[?], TokenWithLoc[?])] = Parser(ctx => Right(((ctx.curT, ctx.nxtT), ctx)))

  private val advance: Parser[Unit] = Parser { ctx => ctx.advance.map { nextCtx => ((), nextCtx) } }

  def eatToken(expected: Token): Parser[TokenWithLoc[?]] =
    Parser.cur.flatMap { received =>
      if received.token == expected then Parser.advance.map(_ => received)
      else ParserErr.UnexpectedToken(received, ExpectedTokens.Single(expected)).toParserFail
    }

  def skipToken(expected: Token): Parser[Unit] =
    eatToken(expected).map(_ => ())
}

extension (err: ParserErr)
  def toParserFail: Parser[Nothing] =
    Parser(_ => Left(err))