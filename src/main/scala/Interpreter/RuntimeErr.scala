package Interpreter

import Lexer.Pos
import SemanticAnalyzer.SemanticErr

enum RuntimeErr() {
  case SomeErr();
}
