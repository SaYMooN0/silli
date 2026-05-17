
@main
def main(): Unit = {
  Lexer.Lexer.printAllTokens("""begin 
                               |    if n <= 1 then
                               |        inter := "empty"
                               |    else
                               |    begin
                               |        inter := cap*(n);    
                               |        CalcFactorial(n - 1, cap*(n-1));
                               |    end;
                               |end;
                               |
                               |begin
                               |    CalcFactorial(4, "-");
                               |end.""".stripMargin)
}

