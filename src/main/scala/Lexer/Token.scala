package Lexer

import TypeSystem.BuiltInType


final case class TokenWithLoc[+T <: Token](token: T, loc: Loc)

type Token =
  BooleanLiteralToken
    | IntegerNumLiteralToken
    | RealNumLiteralToken
    | StringLiteralToken
    | BuiltInTypeNameToken
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

final case class BuiltInTypeNameToken(typeSpec: BuiltInType)

final case class IdentToken(ident: String);

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
  case Div //div keyword

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
  case Begin;
  case End;
  case Var;
  case Program;
  case Procedure;
  case If;
  case Then;
  case Else;
}

type Keyword =
  OpToken.Div.type
    | OpToken.Not.type
    | OpToken.And.type
    | OpToken.Or.type
    | OpToken.Xor.type
    | SyntaxKeywordToken
    | BuiltInTypeNameToken
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
  case BuiltInType.IntegerT.name => Some(BuiltInTypeNameToken(BuiltInType.IntegerT))
  case BuiltInType.RealT.name => Some(BuiltInTypeNameToken(BuiltInType.RealT))
  case BuiltInType.BooleanT.name => Some(BuiltInTypeNameToken(BuiltInType.BooleanT))
  case BuiltInType.StringT.name => Some(BuiltInTypeNameToken(BuiltInType.StringT))
  //BoolLiteralToken
  case "true" => Some(BooleanLiteralToken.True)
  case "false" => Some(BooleanLiteralToken.False)
  //else
  case _ => None
}
