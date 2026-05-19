package Parser

import Lexer.{EndOfReaderReached, IdentToken, Loc, ReadingCtx, SimpleToken, SyntaxKeywordToken, TokenizingErr, BuiltInTypeNameToken}

import java.io.StringReader

def constructAst(str: String): Either[ParserErr, AstRoot] = {
  val parsingCtxRes = ParsingCtx.init(new StringReader(str), Lexer.Lexer.getDefaultReadNextTokenFunc)
  parsingCtxRes match {
    case Right(ctx) => parseProgramAst().run(ctx) match {
      case Left(err) => Left(err)
      case Right(astRoot, _) => Right(astRoot)
    }
    case Left(err) => Left(err)
  }
}


private def parseProgramAst(): Parser[AstRoot] =
  for {
    _ <- Parser.skipToken(SyntaxKeywordToken.Program)
    programName <- parseIdentWithLoc()
    _ <- Parser.skipToken(SimpleToken.SemiColon)
    block <- parseBlock()
    _ <- Parser.skipToken(SimpleToken.Dot)
  } yield AstRoot(programName, block)

private def parseBlock(): Parser[AstBlock] =
  for {
    firstToken <- Parser.cur
    varDecls <- parseVarDecls()
    procDecls <- parseProcDeclAndContinue(List())
    compoundStmt <- parseCompoundStmt()
  } yield AstBlock(varDecls, procDecls, compoundStmt, Loc(firstToken.loc.start, compoundStmt.loc.end))

private def parseVarDecls(): Parser[List[AstVarDecl]] = Parser.cur.flatMap { cur =>
  cur.token match {
    case SyntaxKeywordToken.Var => Parser.skipToken(cur.token).flatMap(_ => parseVarDeclAndContinue(List()))
    case _ => Parser.succeed(List())
  }
}
private def parseVarDeclAndContinue(gathered: List[AstVarDecl]): Parser[List[AstVarDecl]] = {
  for {
    identsWithAnnotation <- parseIdentsListWithTypeAnnotation()
    _ <- Parser.skipToken(SimpleToken.SemiColon)
    declsWithNew = gathered ++ identsWithAnnotation.map(
      (ident, typeAnnotation) => AstVarDecl(ident, typeAnnotation, Loc(ident._2.start, typeAnnotation._2.end))
    )

    result <- Parser.cur.flatMap { cur =>
      cur.token match {
        case _: IdentToken => parseVarDeclAndContinue(declsWithNew)
        case _ => Parser.succeed(declsWithNew)
      }
    }
  } yield result
}

private def parseProcDeclAndContinue(gathered: List[AstProcDecl]): Parser[List[AstProcDecl]] = {
  Parser.cur.flatMap { cur =>
    cur.token match {
      case SyntaxKeywordToken.Procedure => for {
        procedureKw <- Parser.eatToken(cur.token)
        procName <- parseIdentWithLoc()
        formalParams <- parseProcDeclFormalParams()
        _ <- Parser.skipToken(SimpleToken.SemiColon)
        block <- parseBlock()
        lastSemi <- Parser.eatToken(SimpleToken.SemiColon)
        procDeclLoc = Loc(procedureKw.loc.start, lastSemi.loc.end)
        result <- parseProcDeclAndContinue(gathered :+ AstProcDecl(procName, formalParams, block, procDeclLoc))
      } yield result
      case _ => Parser.succeed(gathered)
    }
  }
}
private def parseProcDeclFormalParams(): Parser[List[AstFormalParam]] = {
  Parser.cur.flatMap { cur =>
    cur.token match {
      case SimpleToken.LPar => for {
        _ <- Parser.skipToken(cur.token)
        result <- continueProcDeclFormalParams(List())
        _ <- Parser.skipToken(SimpleToken.RPar)
      } yield result
      case _ => Parser.succeed(List())
    }
  }
}
private def continueProcDeclFormalParams(gathered: List[AstFormalParam]): Parser[List[AstFormalParam]] = for {
  newParams <- parseIdentsListWithTypeAnnotation()
  gatheredWithNew = gathered ++ newParams.map(
    (ident, typeAnnotation) => AstFormalParam(ident, typeAnnotation, Loc(ident._2.start, typeAnnotation._2.end))
  )
  result <- Parser.cur.flatMap { cur =>
    cur.token match {
      case SimpleToken.SemiColon =>
        Parser
          .eatToken(SimpleToken.SemiColon)
          .flatMap(_ => continueProcDeclFormalParams(gatheredWithNew))
      case _ => Parser.succeed(gatheredWithNew)
    }
  }
} yield result

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

//helpers
private def parseIdentWithLoc(): Parser[(Ident, Loc)] =
  Parser.cur.flatMap { received =>
    received.token match {
      case t: IdentToken => Parser.skipToken(t).map(_ => (Ident(t.ident), received.loc))
      case _ => ParserErr.CouldNotParseExpectedConstruct(received, ExpectedConstruct.Ident).toParserFail
    }
  }
private def parseTypeSpecWithLoc(): Parser[(TypeSystem.BuiltInType, Loc)] =
  Parser.cur.flatMap { received =>
    received.token match {
      case t: BuiltInTypeNameToken => Parser.skipToken(t).map(_ => (t.typeSpec, received.loc))
      case _ => ParserErr.CouldNotParseExpectedConstruct(received, ExpectedConstruct.TypeSpec).toParserFail
    }
  }
private def parseNonEmptyIdentsList(): Parser[List[(Ident, Loc)]] =
  for {
    head <- parseIdentWithLoc()
    tail <- parseIdentsListTail(List())
  } yield head :: tail


private def parseIdentsListTail(gathered: List[(Ident, Loc)]): Parser[List[(Ident, Loc)]] =
  Parser.curAndNxt.flatMap { case (cur, nxt) =>
    (cur.token, nxt.token) match {
      case (SimpleToken.Comma, _: IdentToken) =>
        for {
          _ <- Parser.skipToken(SimpleToken.Comma)
          identWithLoc <- parseIdentWithLoc()
          result <- parseIdentsListTail(gathered :+ identWithLoc)
        } yield result

      case _ => Parser.succeed(gathered)
    }
  }

private type IdentWithTypeAnnotation = ((Ident, Loc), (TypeSystem.BuiltInType, Loc))
private def parseIdentsListWithTypeAnnotation(): Parser[List[IdentWithTypeAnnotation]] =
  for {
    idents <- parseNonEmptyIdentsList()
    _ <- Parser.skipToken(SimpleToken.Colon)
    typeSpec <- parseTypeSpecWithLoc()
  } yield idents.map { identWithLoc =>
    (identWithLoc, typeSpec)
  }

