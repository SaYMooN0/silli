//package Lexer
//
//object NumLiteralReader {
//  private class NumLiteralReaderCtx(
//                                     val capRev: List[Char],
//                                     val literalStartPos: Pos,
//                                     val dotAdded: Boolean,
//                                     val lexerCtx: ReadingCtx
//                                   )
//
//  private type NumLiteral = IntegerNumLiteralToken | RealNumLiteralToken
//
//  enum NumLiteralReadingError {
//    case CouldNotTransform
//  }
//
//  private def constructLiteralOnOut(
//                                     ctx: NumLiteralReaderCtx
//                                   ): Either[NumLiteralReadingError, (NumLiteral, Loc, ReadingCtx)] = {
//
//    val numInStr = ctx.capRev.reverse.mkString
//
//    val loc = Loc(
//      start = ctx.literalStartPos,
//      end = ctx.lexerCtx.current
//    )
//
//    if (ctx.dotAdded) {
//      numInStr.toDoubleOption match {
//        case Some(value) =>
//          Right((RealNumLiteralToken(value), loc, ctx.lexerCtx))
//
//        case None =>
//          Left(NumLiteralReadingError.CouldNotTransform)
//      }
//    } else {
//      numInStr.toIntOption match {
//        case Some(value) =>
//          Right((IntegerNumLiteralToken(value), loc, ctx.lexerCtx))
//
//        case None =>
//          Left(NumLiteralReadingError.CouldNotTransform)
//      }
//    }
//  }
//}
//
//  /*
//  * let private constructLiteralOnOut ctx : Result<NumLiteral * Loc * TokenizingCtx, string> =
//        let numInStr = ctx.capRev |> List.rev |> List.toArray |> String
//
//        let loc =
//            { startP = ctx.literalStartPos
//              endP = ctx.tokenizingCtx.currentPos }
//
//        if ctx.dotAdded then
//            match Double.TryParse numInStr with
//            | true, v -> Ok(NumLiteral.Real(v), loc, ctx.tokenizingCtx)
//            | false, _ -> Error numInStr
//        else
//            match Int32.TryParse numInStr with
//            | true, v -> Ok(NumLiteral.Integer(v), loc, ctx.tokenizingCtx)
//            | false, _ -> Error numInStr
//
//    let private advanceWithNewDigit (ctx) (digitCh) =
//        { ctx with
//            capRev = digitCh :: ctx.capRev
//            tokenizingCtx = TokenizingCtx.advance ctx.tokenizingCtx }
//
//    let private advanceWithDot (ctx) =
//        { ctx with
//            capRev = '.' :: ctx.capRev
//            dotAdded = true
//            tokenizingCtx = TokenizingCtx.advance ctx.tokenizingCtx }
//
//    let rec private continueLiteralTillOut (ctx: NumberExtractingCtx) =
//        let curCharClass = getCharClass ctx.tokenizingCtx.currentChar
//
//        match curCharClass, ctx.dotAdded with
//        | CharClass.Other digit, _ when Char.IsDigit digit -> continueLiteralTillOut (advanceWithNewDigit ctx digit)
//        | CharClass.Dot, false -> continueLiteralTillOut (advanceWithDot ctx)
//        // else
//        | _, _ -> constructLiteralOnOut ctx
//
//    let readNumLiteral (firstChar: char) (ctx) =
//        if not (Char.IsDigit firstChar) then
//            failwith "Num token must start with a digit"
//        else
//            let numLitCtx =
//                { capRev = [ firstChar ]
//                  literalStartPos = ctx.currentPos
//                  dotAdded = false
//                  tokenizingCtx = TokenizingCtx.advance ctx }
//
//            let res = continueLiteralTillOut numLitCtx
//
//            match res with
//            | Ok ok -> ok
//            | Error numInStr -> failwith $"Could not transform {numInStr} into {nameof NumLiteral}"
//  * 
//  * */
//}
