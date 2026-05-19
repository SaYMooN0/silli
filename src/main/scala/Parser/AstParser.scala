package Parser

import Lexer.{IdentToken, Loc, SimpleToken, SyntaxKeywordToken}

private def program(): Parser[AstRoot] =
  for {
    _ <- Parser.skipToken(SyntaxKeywordToken.Program)
    programName <- parserIdentWithLoc()
    _ <- Parser.skipToken(SimpleToken.SemiColon)
    block <- parseBlock()
    _ <- Parser.skipToken(SimpleToken.Dot)
  } yield AstRoot(programName, block)
private def parserIdentWithLoc(): Parser[(Ident, Loc)] =
  Parser.cur.flatMap { received =>
    received.token match {
      case t: IdentToken => {
        Parser.eatToken(t)
        Parser.succeed(Ident(t.ident), received.loc)
      }
      case _ => ParserErr.CouldNotParseExpectedConstruct(received, ExpectedConstruct.Ident).toParserFail
    }
  }

private def parseBlock(): Parser[AstBlock] =
  for {
    firstToken <- Parser.cur
    varDecls <- parseVarDecls()
    procDecls <- parseProcDecls()
    compoundStmt <- parseCompoundStmt()
  } yield AstBlock(varDecls, procDecls, compoundStmt, Loc(firstToken.loc.start, compoundStmt.loc.end))

private def parseVarDecls(): Parser[List[AstVarDecl]] = ???
private def parseProcDecls(): Parser[List[AstProcDecl]] = ???

//stmts
private def parseStmt(): Parser[AstStmt] =
  Parser.curAndNxt.flatMap { (cur, nxt) =>
    (cur.token, nxt.token) match {
      case (SyntaxKeywordToken.Begin, _) => parseCompoundStmt()
      case (SyntaxKeywordToken.If, _) => parseIfStmt()
      case (ident: IdentToken, SimpleToken.LPar) => parseProcCallStmt()
      case (ident: IdentToken, SimpleToken.Assign) => parseAssignStmt()
      case _ => ParserErr.CouldNotParseExpectedConstruct(cur, ExpectedConstruct.Stmt).toParserFail
    }
  }
private def parseCompoundStmt(): Parser[AstCompoundStmt] = ???
private def parseIfStmt(): Parser[AstIfStmt] = ???
private def parseProcCallStmt(): Parser[AstProcCallStmt] = ???
private def parseAssignStmt(): Parser[AstAssignStmt] = ???
//expr