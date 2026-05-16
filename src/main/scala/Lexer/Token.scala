package Lexer

import TypeSystem.TypeSpec


type TokenWithLoc = (Token, Loc)
type Token =
  BoolLiteralToken
    | IntegerNumLiteralToken
    | RealNumLiteralToken
    | StringLiteralToken
    | TypeNameToken
    | IdentToken
    | SimpleToken
    | OpToken
    | SyntaxKeyWordToken


enum BoolLiteralToken {
  case True;
  case False;
}

class IntegerNumLiteralToken(value: Int)

class RealNumLiteralToken(value: Double)

class StringLiteralToken(value: String)

class TypeNameToken(typeSpec: TypeSpec)

class IdentToken(ident: String);

enum SimpleToken {
  case Assign; //:=
  case Colon; //:
  case SemiColon; //;
  case Dot; //.
  case Comma; //,
  case LPar; //(
  case RPar; //)
}

enum OpToken {
  //not 
  case Not
  //basic arithmetic
  case Plus // +
  case Minus // -
  case Mul // *

  //div
  case RealDiv // /
  case Div

  // comparison
  case Equal // =
  case NotEqual // <>
  case Less // <
  case LessOrEqual // <=
  case Greater // >
  case GreaterOrEqual // >=

  // rel
  case And
  case Or
  case Xor
}

enum SyntaxKeyWordToken {
  case End
  case Begin
  case Var
  case Program
  case Procedure
  case If
  case Then
  case Else
}

