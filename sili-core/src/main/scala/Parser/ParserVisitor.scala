package Parser

import Lexer.*

import java.io.StringReader

def constructAst(str: String): Either[ParserErr, AstRoot] = {
  val parsingCtxRes = ParsingCtx.init(new StringReader(str), Lexer.getDefaultReadNextTokenFunc)
  parsingCtxRes match {
    case Right(ctx) => parseProgramAst().run(ctx) match {
      case Left(err)         => Left(err)
      case Right(astRoot, _) => Right(astRoot)
    }
    case Left(err)  => Left(err)
  }
}


private def parseProgramAst(): Parser[AstRoot] = {
  for {
    _ <- Parser.eatToken(SyntaxKeywordToken.Program)
    programName <- parseIdentWithLoc()
    _ <- Parser.eatToken(SimpleToken.SemiColon)
    block <- parseBlock()
    _ <- Parser.eatTokenAsLastInReader(SimpleToken.Dot)
  } yield AstRoot(programName, block)
}

private def parseBlock(): Parser[AstBlock] = {
  for {
    firstToken <- Parser.cur
    decls <- parseBlockDecls()
    compoundStmt <- parseCompoundStmt()
  } yield AstBlock(decls, compoundStmt, Loc(firstToken.loc.start, compoundStmt.loc.end))
}
private def parseBlockDecls(): Parser[List[AstDeclarationItem]] = parseBlockDeclsTail(List.empty)

private def parseBlockDeclsTail(gatheredReversed: List[AstDeclarationItem]): Parser[List[AstDeclarationItem]] = {
  Parser.cur.flatMap { cur =>
    cur.token match {
      case DeclarationKeywordToken.Var => for {
        decl <- parseVarGroupDecl()
        result <- parseBlockDeclsTail(decl :: gatheredReversed)
      } yield result

      case DeclarationKeywordToken.Procedure => for {
        decl <- parseProcDecl()
        result <- parseBlockDeclsTail(decl :: gatheredReversed)
      } yield result

      case DeclarationKeywordToken.Function => for {
        decl <- parseFuncDecl()
        result <- parseBlockDeclsTail(decl :: gatheredReversed)
      } yield result

      case _ => Parser.succeed(gatheredReversed.reverse)
    }
  }
}
private def parseVarGroupDecl(): Parser[AstDeclarationItem.AstVarGroupDecl] = for {
  _ <- Parser.eatToken(DeclarationKeywordToken.Var)
  varDecls <- parseTypedVarsDeclAndContinue(List.empty)
} yield AstDeclarationItem.AstVarGroupDecl(varDecls)

private def parseTypedVarsDecl(): Parser[AstTypedVarsDecl] = for {
  idents <- parseNonEmptyIdentsList()
  _ <- Parser.eatToken(SimpleToken.Colon)
  typeAnnotation <- parseTypeSpecWithLoc()
  semi <- Parser.eatToken(SimpleToken.SemiColon)
  varRefs = idents.map { case (ident, loc) => AstVarRef(ident, loc) }
  declLoc = Loc(varRefs.head.loc.start, semi.loc.end)
} yield AstTypedVarsDecl(varRefs, typeAnnotation, declLoc)

private def parseTypedVarsDeclAndContinue(gatheredReversed: List[AstTypedVarsDecl]): Parser[List[AstTypedVarsDecl]] = {
  for {
    decl <- parseTypedVarsDecl()
    result <- Parser.cur.flatMap {
      _.token match {
        case _: IdentToken => parseTypedVarsDeclAndContinue(decl :: gatheredReversed)
        case _             => Parser.succeed((decl :: gatheredReversed).reverse)
      }
    }
  } yield result
}
private def parseProcDecl(): Parser[AstDeclarationItem.AstProcDecl] = for {
  procedureKw <- Parser.eatToken(DeclarationKeywordToken.Procedure)
  procName <- parseIdentWithLoc()
  formalParams <- parseFormalParams()
  _ <- Parser.eatToken(SimpleToken.SemiColon)
  block <- parseBlock()
  lastSemi <- Parser.eatToken(SimpleToken.SemiColon)
  procDeclLoc = Loc(procedureKw.loc.start, lastSemi.loc.end)
} yield AstDeclarationItem.AstProcDecl(procName, formalParams, block, procDeclLoc)

private def parseFuncDecl(): Parser[AstDeclarationItem.AstFuncDecl] = for {
  functionKw <- Parser.eatToken(DeclarationKeywordToken.Function)
  funcName <- parseIdentWithLoc()
  formalParams <- parseFormalParams()
  _ <- Parser.eatToken(SimpleToken.Colon)
  typeAnnotation <- parseTypeSpecWithLoc()
  _ <- Parser.eatToken(SimpleToken.SemiColon)
  block <- parseBlock()
  lastSemi <- Parser.eatToken(SimpleToken.SemiColon)
  funcDeclLoc = Loc(functionKw.loc.start, lastSemi.loc.end)
} yield AstDeclarationItem.AstFuncDecl(funcName, formalParams, block, typeAnnotation, funcDeclLoc)

private def parseFormalParams(): Parser[List[AstFormalParam]] = {
  Parser.cur.flatMap { cur =>
    cur.token match {
      case SimpleToken.LPar => for {
        _ <- Parser.eatToken(cur.token)
        result <- continueProcDeclFormalParams(List())
        _ <- Parser.eatToken(SimpleToken.RPar)
      } yield result

      case _ => Parser.succeed(List.empty)
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
      case SimpleToken.SemiColon => Parser
        .eatToken(SimpleToken.SemiColon)
        .flatMap(_ => continueProcDeclFormalParams(gatheredWithNew))

      case _ => Parser.succeed(gatheredWithNew)
    }
  }
} yield result

//statements
private def parseStmt(): Parser[AstStmt] =
  Parser.curAndNxt.flatMap { (cur, nxt) =>
    (cur.token, nxt.token) match {
      case (SyntaxKeywordToken.Begin, _)           => parseCompoundStmt()
      case (SyntaxKeywordToken.If, _)              => parseIfStmt()
      case (ident: IdentToken, SimpleToken.LPar)   => parseProcCallStmt(ident)
      case (ident: IdentToken, SimpleToken.Assign) => parseAssignStmt(ident)
      case _                                       => ParserErr.CouldNotParseExpectedConstruct(cur, ExpectedConstruct.Stmt).toParserFail
    }
  }

private def parseCompoundStmt(): Parser[AstCompoundStmt] =
  for {
    beginKw <- Parser.eatToken(SyntaxKeywordToken.Begin)
    allStmts <- parseStmtsListTailTillEndKw(List(), isDividerExpected = false)
    endKw <- Parser.eatToken(SyntaxKeywordToken.End)
  } yield AstCompoundStmt(allStmts, Loc(beginKw.loc.start, endKw.loc.end))

private def parseIfStmt(): Parser[AstIfStmt] = {
  for {
    ifKw <- Parser.eatToken(SyntaxKeywordToken.If)
    conditionExpr <- parseExpr()
    thenKw <- Parser.eatToken(SyntaxKeywordToken.Then)
    thenStmt <- parseOptionalThenStmt()
    elseStmt <- Parser.cur.flatMap { cur =>
      cur.token match {
        case SyntaxKeywordToken.Else =>
          for {
            _ <- Parser.eatToken(SyntaxKeywordToken.Else)
            stmt <- parseStmt()
          } yield Some(stmt)

        case _ => Parser.succeed(None)
      }
    }
    fullStmtEndPos =
      elseStmt
        .map(_.loc.end)
        .orElse(thenStmt.map(_.loc.end))
        .getOrElse(thenKw.loc.end)

  } yield AstIfStmt(conditionExpr, thenStmt, elseStmt, Loc(ifKw.loc.start, fullStmtEndPos))
}
private def parseOptionalThenStmt(): Parser[Option[AstStmt]] = {
  Parser.curAndNxt.flatMap { (cur, nxt) =>
    (cur.token, nxt.token) match {
      case (SyntaxKeywordToken.Else, _)                     => Parser.succeed(None)
      case (SimpleToken.SemiColon, SyntaxKeywordToken.Else) => Parser.eatToken(SimpleToken.SemiColon).map(_ => None)
      case (SimpleToken.SemiColon, _)                       => Parser.succeed(None)
      case _                                                => parseStmt().map(stmt => Some(stmt))
    }
  }
}

private def parseProcCallStmt(ident: IdentToken): Parser[AstProcCallStmt] = for {
  identWithLoc <- Parser.eatToken(ident)
  _ <- Parser.eatToken(SimpleToken.LPar)
  params <- parseProcCallParamsList()
  rPar <- Parser.eatToken(SimpleToken.RPar)

} yield AstProcCallStmt((Ident(ident.ident), identWithLoc.loc), params, Loc(identWithLoc.loc.start, rPar.loc.end))

private def parseProcCallParamsList(): Parser[List[AstExpr]] = Parser.cur.flatMap { cur =>
  cur.token match {
    case SimpleToken.RPar => Parser.succeed(List())
    case _                => for {
      exprListHead <- parseExpr()
      allExpr <- parseProcCallParamsTail(List(exprListHead))
    } yield allExpr
  }
}
private def parseProcCallParamsTail(gathered: List[AstExpr]): Parser[List[AstExpr]] = Parser.cur.flatMap { cur =>
  cur.token match {
    case SimpleToken.Comma => for {
      _ <- Parser.eatToken(cur.token)
      newExpr <- parseExpr()
      allExpr <- parseProcCallParamsTail(gathered :+ newExpr)
    } yield allExpr
    case _                 => Parser.succeed(gathered)
  }
}

private def parseAssignStmt(ident: IdentToken): Parser[AstAssignStmt] = for {
  identWithLoc <- Parser.eatToken(ident)
  _ <- Parser.eatToken(SimpleToken.Assign)
  expr <- parseExpr()
} yield AstAssignStmt(AstVarRef(Ident(ident.ident), identWithLoc.loc), expr, Loc(identWithLoc.loc.start, expr.loc.end))
//expressions
private def parseExpr(): Parser[AstExpr] = parseOrExpr()

private def parseOrExpr(): Parser[AstExpr] = parseBinOpChain(
  parseAndExpr,
  mapTokenToOp = {
    case OpToken.Or  => Some(TypeSystem.LogicBinOps.Or)
    case OpToken.Xor => Some(TypeSystem.LogicBinOps.Xor)
    case _           => None
  })

private def parseAndExpr(): Parser[AstExpr] = parseBinOpChain(
  parseRelExpr,
  mapTokenToOp = {
    case OpToken.And => Some(TypeSystem.LogicBinOps.And)
    case _           => None
  })

private def parseRelExpr(): Parser[AstExpr] = {
  val mapTokenToRelOp: Token => Option[TypeSystem.BinOp] = {
    case OpToken.Equal          => Some(TypeSystem.EqualityBinOps.Equal)
    case OpToken.NotEqual       => Some(TypeSystem.EqualityBinOps.NotEqual)
    case OpToken.Less           => Some(TypeSystem.ComparisonBinOps.Less)
    case OpToken.LessOrEqual    => Some(TypeSystem.ComparisonBinOps.LessOrEqual)
    case OpToken.Greater        => Some(TypeSystem.ComparisonBinOps.Greater)
    case OpToken.GreaterOrEqual => Some(TypeSystem.ComparisonBinOps.GreaterOrEqual)
    case _                      => None
  }
  for {
    left <- parseAddExpr()
    result <- Parser.cur.flatMap { opToken =>
      mapTokenToRelOp(opToken.token) match {
        case None        => Parser.succeed(left)
        case Some(binOp) => for {
          _ <- Parser.eatToken(opToken.token)
          right <- parseAddExpr()
        } yield AstBinOp(left, right, (binOp, opToken.loc), Loc(left.loc.start, right.loc.end))

      }
    }
  } yield result
}
private def parseAddExpr(): Parser[AstExpr] = parseBinOpChain(
  parseMulExpr,
  mapTokenToOp = {
    case OpToken.Plus  => Some(TypeSystem.ArithmeticBinOps.Add)
    case OpToken.Minus => Some(TypeSystem.ArithmeticBinOps.Sub)
    case _             => None
  })
private def parseMulExpr(): Parser[AstExpr] = parseBinOpChain(
  parseUnaryExpr,
  mapTokenToOp = {
    case OpToken.Mul     => Some(TypeSystem.ArithmeticBinOps.Mul)
    case OpToken.RealDiv => Some(TypeSystem.RealDivBinOp)
    case OpToken.Div     => Some(TypeSystem.IntDivBinOp)
    case _               => None
  })

private def parseUnaryExpr(): Parser[AstExpr] = {
  val mapTokenToUnaryOp: Token => Option[TypeSystem.UnOp] = {
    case OpToken.Not   => Some(TypeSystem.UnOp.Not)
    case OpToken.Minus => Some(TypeSystem.UnOp.Minus)
    case OpToken.Plus  => Some(TypeSystem.UnOp.Plus)
    case _             => None
  }

  Parser.cur.flatMap { opToken =>
    mapTokenToUnaryOp(opToken.token) match {
      case None       => parsePrimaryExpr()
      case Some(unOp) => for {
        _ <- Parser.eatToken(opToken.token)
        inner <- parseUnaryExpr()
      } yield AstUnOp(inner, (unOp, opToken.loc), Loc(opToken.loc.start, inner.loc.end))
    }
  }
}
private def parsePrimaryExpr(): Parser[AstExpr] = {
  Parser.cur.flatMap { cur =>
    cur.token match {
      case l: RealNumLiteralToken    => Parser.skipAndSucceed(l, AstRealLiteral(l.value, cur.loc))
      case l: IntegerNumLiteralToken => Parser.skipAndSucceed(l, AstIntegerLiteral(l.value, cur.loc))
      case l: StringLiteralToken     => Parser.skipAndSucceed(l, AstStringLiteral(l.value, cur.loc))
      case BooleanLiteralToken.True  => Parser.skipAndSucceed(BooleanLiteralToken.True, AstBooleanLiteral(true, cur.loc))
      case BooleanLiteralToken.False => Parser.skipAndSucceed(BooleanLiteralToken.False, AstBooleanLiteral(false, cur.loc))
      case ident: IdentToken         => Parser.skipAndSucceed(ident, AstVarRef(Ident(ident.ident), cur.loc))
      case SimpleToken.LPar          => for {
        lPar <- Parser.eatToken(cur.token)
        innerExpr <- parseExpr()
        rPar <- Parser.eatToken(SimpleToken.RPar)
      } yield innerExpr
      case _                         => ParserErr.CouldNotParseExpectedConstruct(cur, ExpectedConstruct.PrimaryExpr).toParserFail
    }
  }
}

//helpers
private def parseIdentWithLoc(): Parser[(Ident, Loc)] =
  Parser.cur.flatMap { received =>
    received.token match {
      case t: IdentToken => Parser.skipAndSucceed(t, (Ident(t.ident), received.loc))
      case _             => ParserErr.CouldNotParseExpectedConstruct(received, ExpectedConstruct.Ident).toParserFail
    }
  }

private def parseTypeSpecWithLoc(): Parser[(Ident, Loc)] =
  Parser.cur.flatMap { received =>
    received.token match {
      case t: BuiltInTypeNameToken => Parser.skipAndSucceed(t, (Ident(t.typeSpec.name), received.loc))
      case ident: IdentToken       => Parser.skipAndSucceed(ident, (Ident(ident.ident), received.loc))
      case _                       => ParserErr.CouldNotParseExpectedConstruct(received, ExpectedConstruct.TypeSpec).toParserFail
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
          _ <- Parser.eatToken(SimpleToken.Comma)
          identWithLoc <- parseIdentWithLoc()
          result <- parseIdentsListTail(gathered :+ identWithLoc)
        } yield result

      case _ => Parser.succeed(gathered)
    }
  }

private type IdentWithTypeAnnotation = ((Ident, Loc), (Ident, Loc))

private def parseIdentsListWithTypeAnnotation(): Parser[List[IdentWithTypeAnnotation]] =
  for {
    idents <- parseNonEmptyIdentsList()
    _ <- Parser.eatToken(SimpleToken.Colon)
    typeSpec <- parseTypeSpecWithLoc()
  } yield idents.map { identWithLoc =>
    (identWithLoc, typeSpec)
  }


private def parseStmtsListTailTillEndKw(gathered: List[AstStmt], isDividerExpected: Boolean): Parser[List[AstStmt]] =
  Parser.cur.flatMap { cur =>
    (isDividerExpected, cur.token) match {
      case (_, SyntaxKeywordToken.End)    => Parser.succeed(gathered)
      case (true, SimpleToken.SemiColon)  =>
        for {
          semi <- Parser.eatToken(SimpleToken.SemiColon)
          result <- parseStmtsListTailTillEndKw(gathered, isDividerExpected = false)
        } yield result
      case (false, SimpleToken.SemiColon) =>
        for {
          semi <- Parser.eatToken(SimpleToken.SemiColon)
          result <- parseStmtsListTailTillEndKw(gathered, isDividerExpected = false)
        } yield result
      case (true, received)               => ParserErr.UnexpectedToken(
        cur, ExpectedTokens.OneOf(Set(SimpleToken.SemiColon, SyntaxKeywordToken.End))
      ).toParserFail
      case (false, _)                     =>
        for {
          stmt <- parseStmt()
          result <- parseStmtsListTailTillEndKw(gathered :+ stmt, isDividerExpected = true)
        } yield result
    }
  }

private def parseBinOpChain(partParser: () => Parser[AstExpr], mapTokenToOp: Token => Option[TypeSystem.BinOp]) = for {
  left <- partParser()
  result <- parseBinOpChainTail(left, partParser, mapTokenToOp)
} yield result

private def parseBinOpChainTail(
                                 left: AstExpr,
                                 partParser: () => Parser[AstExpr],
                                 mapTokenToOp: Token => Option[TypeSystem.BinOp]
                               ): Parser[AstExpr] =
  Parser.cur.flatMap { opToken =>
    mapTokenToOp(opToken.token) match {
      case None     => Parser.succeed(left)
      case Some(op) => for {
        _ <- Parser.eatToken(opToken.token)
        right <- partParser()
        node = AstBinOp(left, right, (op, opToken.loc), Loc(left.loc.start, right.loc.end))
        result <- parseBinOpChainTail(node, partParser, mapTokenToOp)
      } yield result
    }
  }
