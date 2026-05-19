package Lexer

import TypeSystem.TypeSpec


final case class TokenWithLoc[+T <: Token](token: T, loc: Loc)

type Token =
  BooleanLiteralToken
    | IntegerNumLiteralToken
    | RealNumLiteralToken
    | StringLiteralToken
    | TypeNameToken
    | IdentToken
    | SimpleToken
    | OpToken
    | SyntaxKeywordToken


enum BooleanLiteralToken {
  case True;
  case False;
}

final case class IntegerNumLiteralToken(value: Int)
final case class RealNumLiteralToken(value: Double)
final case class StringLiteralToken(value: String)

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

enum SyntaxKeywordToken {
  case End
  case Begin
  case Var
  case Program
  case Procedure
  case If
  case Then
  case Else
}

type Keyword =
  OpToken.Div.type
    | OpToken.Not.type
    | OpToken.And.type
    | OpToken.Or.type
    | OpToken.Xor.type
    | SyntaxKeywordToken
    | TypeNameToken
    | BooleanLiteralToken

def mapToKeyword(str: String): Option[Keyword] = str match {
  //OpToken
  case "div" => Some(OpToken.Div)
  case "not" => Some(OpToken.Not)
  case "and" => Some(OpToken.And)
  case "or" => Some(OpToken.Or)
  case "xor" => Some(OpToken.Xor)
  //SyntaxKeywordToken
  case "end" => Some(SyntaxKeywordToken.End)
  case "begin" => Some(SyntaxKeywordToken.Begin)
  case "var" => Some(SyntaxKeywordToken.Var)
  case "program" => Some(SyntaxKeywordToken.Program)
  case "procedure" => Some(SyntaxKeywordToken.Procedure)
  case "if" => Some(SyntaxKeywordToken.If)
  case "then" => Some(SyntaxKeywordToken.Then)
  case "else" => Some(SyntaxKeywordToken.Else)
  //TypeNameToken
  case TypeSpec.IntegerT.name => Some(TypeNameToken(TypeSpec.IntegerT))
  case TypeSpec.RealT.name => Some(TypeNameToken(TypeSpec.RealT))
  case TypeSpec.BooleanT.name => Some(TypeNameToken(TypeSpec.BooleanT))
  case TypeSpec.StringT.name => Some(TypeNameToken(TypeSpec.StringT))
  //BoolLiteralToken
  case "true" => Some(BooleanLiteralToken.True)
  case "false" => Some(BooleanLiteralToken.False)
  //else
  case _ => None
}
