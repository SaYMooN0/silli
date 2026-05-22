package Parser

import _root_.Lexer.*


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

  val nxt: Parser[TokenWithLoc[?] | EndOfReaderReached] = Parser(ctx => Right((ctx.nxtT, ctx)))

  val curAndNxt: Parser[(TokenWithLoc[?], TokenWithLoc[?] | EndOfReaderReached)] =
    Parser(ctx => Right(((ctx.curT, ctx.nxtT), ctx)))

  private val advance: Parser[Unit] = Parser { ctx => ctx.advance.map { nextCtx => ((), nextCtx) } }

  def eatToken(expected: Token): Parser[TokenWithLoc[?]] =
    Parser.cur.flatMap { received =>
      if received.token == expected
      then Parser.advance.map(_ => received)
      else ParserErr.UnexpectedToken(received, ExpectedTokens.Single(expected)).toParserFail
    }

  def eatTokenAsLastInReader(expected: Token): Parser[TokenWithLoc[?]] =
    Parser.curAndNxt.flatMap { (cur, nxt) =>
      if cur.token != expected then ParserErr.UnexpectedToken(cur, ExpectedTokens.Single(expected)).toParserFail
      else nxt match {
        case _: EndOfReaderReached => Parser.succeed(cur)
        case receivedNext: TokenWithLoc[?] => ParserErr.ExpectedEndOfReader(receivedNext).toParserFail
      }
    }

  def skipAndSucceed[A](t: Token, value: A): Parser[A] = Parser.eatToken(t).map(_ => value)
}

extension (err: ParserErr)
  def toParserFail: Parser[Nothing] =
    Parser(_ => Left(err))

extension (tokenOrEnd: TokenWithLoc[?] | EndOfReaderReached)
  def token: Token | EndOfReaderReached =
    tokenOrEnd match
      case t: TokenWithLoc[?] => t.token
      case end: EndOfReaderReached => end
