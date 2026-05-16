import TypeSystem.TypeSpec

enum KeyWordToken {
  case End
  case Begin
  case Var
  case Program

  case TypeName(typeSpec: TypeSpec)

  case Div
  case Procedure
  case If
  case Then
  case Else
  case True
  case False
  case Not
  case And
  case Or
  case Xor
}

def toToken(typeSpec: TypeSpec): KeyWordToken = KeyWordToken.TypeName(typeSpec)