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
    | Keyword


enum BooleanLiteralToken {
  case True
  case False
}

final case class IntegerNumLiteralToken(value: Int)

final case class RealNumLiteralToken(value: Double)

final case class StringLiteralToken(value: String)

final case class BuiltInTypeNameToken(typeSpec: BuiltInType)

final case class IdentToken(ident: String)

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
  case Mod //mod keyword

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
  case Begin
  case End
  case Program
  case If
  case Then
  case Else
}

enum DeclarationKeywordToken {
  case Var
  case Procedure
  case Function
}

type Keyword =
  OpToken
    | SyntaxKeywordToken
    | DeclarationKeywordToken
    | BuiltInTypeNameToken
    | BooleanLiteralToken

def mapToKeyword(str: String): Option[Keyword] = str match {
  //OpToken
  case "div" => Some(OpToken.Div)
  case "mod" => Some(OpToken.Mod)
  case "not" => Some(OpToken.Not)
  case "and" => Some(OpToken.And)
  case "or"  => Some(OpToken.Or)
  case "xor" => Some(OpToken.Xor)
  //DeclarationKeywordToken
  case "var"       => Some(DeclarationKeywordToken.Var)
  case "procedure" => Some(DeclarationKeywordToken.Procedure)
  case "function"  => Some(DeclarationKeywordToken.Function)
  //SyntaxKeywordToken
  case "end"     => Some(SyntaxKeywordToken.End)
  case "begin"   => Some(SyntaxKeywordToken.Begin)
  case "program" => Some(SyntaxKeywordToken.Program)
  case "if"      => Some(SyntaxKeywordToken.If)
  case "then"    => Some(SyntaxKeywordToken.Then)
  case "else"    => Some(SyntaxKeywordToken.Else)
  //TypeNameToken
  case BuiltInType.IntegerT.name => Some(BuiltInTypeNameToken(BuiltInType.IntegerT))
  case BuiltInType.RealT.name    => Some(BuiltInTypeNameToken(BuiltInType.RealT))
  case BuiltInType.BooleanT.name => Some(BuiltInTypeNameToken(BuiltInType.BooleanT))
  case BuiltInType.StringT.name  => Some(BuiltInTypeNameToken(BuiltInType.StringT))
  //BoolLiteralToken
  case "true"  => Some(BooleanLiteralToken.True)
  case "false" => Some(BooleanLiteralToken.False)
  //else
  case _ => None
}
