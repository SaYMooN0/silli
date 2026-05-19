package Parser

import Lexer.{Pos, Token, TokenWithLoc}

enum ParserErr(val pos: Pos) {
  case UnexpectedToken(
                        received: TokenWithLoc[?],
                        expected: ExpectedTokens
                      ) extends ParserErr(received.loc.start)

  case CouldNotParseExpectedConstruct(
                                       receivedToken: TokenWithLoc[?],
                                       expectedNode: ExpectedConstruct
                                     ) extends ParserErr(receivedToken.loc.start)

  case TokenizingFailed(err: Lexer.TokenizingErr) extends ParserErr(err.pos)

  case UnexpectedEndOfReader(lastToken: TokenWithLoc[?]) extends ParserErr(lastToken.loc.end)
  case EmptyReader(override val pos: Pos) extends ParserErr(pos)
}

enum ExpectedTokens {
  case Single(token: Token)
  case OneOf(tokens: Set[Token])
}

sealed trait ExpectedConstruct

object ExpectedConstruct {
  case object Ident extends ExpectedConstruct
  case object TypeSpec extends ExpectedConstruct
  case object Expr extends ExpectedConstruct
  case object Stmt extends ExpectedConstruct
}