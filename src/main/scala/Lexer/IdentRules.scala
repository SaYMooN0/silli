package Lexer

object IdentRules {
  def isCorrectIdentStarter(ch: Char) = ch.isLetter || ch == '_'

  def canBeInIdentifier(ch: Char) = ch.isDigit || isCorrectIdentStarter(ch)
}
