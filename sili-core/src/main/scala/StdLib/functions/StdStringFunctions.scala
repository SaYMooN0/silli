package StdLib.functions

import SemanticAnalyzer.TypeSymbol
import StdLib.*
import TypeSystem.Value

private def stringExpected(funcName: String, expected: String, actual: List[Value]) =
  Left(StdLibCallErrMsg(s"Built-in function '$funcName' expected $expected, but got: $actual"))

private def validateIndex(
                           funcName: String,
                           index: Int,
                           min: Int,
                           max: Int
                         ): Either[StdLibCallErrMsg, Unit] = {
  if index < min || index > max then
    Left(StdLibCallErrMsg(s"Built-in function '$funcName' got invalid index: $index. Expected index from $min to $max."))
  else Right(())
}

private[StdLib] def StdStringLength = initStdFunction(
  "stringLength",
  List(("value", TypeSymbol.StringSym)),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value)) => Right(Value.IntegerValue(value.length))
      case other                          => stringExpected("stringLength", "one string argument", other)
    }
  }
)

private[StdLib] def StdStringIndexOf = initStdFunction(
  "stringIndexOf",
  List(
    ("value", TypeSymbol.StringSym),
    ("part", TypeSymbol.StringSym)
  ),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value), Value.StringValue(part)) => Right(Value.IntegerValue(value.indexOf(part)))
      case other                                                   => stringExpected("stringIndexOf", "two string arguments", other)
    }
  }
)

private[StdLib] def StdStringLastIndexOf = initStdFunction(
  "stringLastIndexOf",
  List(
    ("value", TypeSymbol.StringSym),
    ("part", TypeSymbol.StringSym)
  ),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value), Value.StringValue(part)) => Right(Value.IntegerValue(value.lastIndexOf(part)))
      case other                                                   => stringExpected("stringLastIndexOf", "two string arguments", other)
    }
  }
)

private[StdLib] def StdStringLeftUntilIndex = initStdFunction(
  "stringLeftUntilIndex",
  List(
    ("value", TypeSymbol.StringSym),
    ("index", TypeSymbol.IntegerSym)
  ),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value), Value.IntegerValue(index)) =>
        validateIndex("stringLeftUntilIndex", index, 0, value.length).map { _ =>
          Value.StringValue(value.take(index))
        }

      case other => stringExpected("stringLeftUntilIndex", "one string argument and one integer argument", other)
    }
  }
)

private[StdLib] def StdStringRightFromIndex = initStdFunction(
  "stringRightFromIndex",
  List(
    ("value", TypeSymbol.StringSym),
    ("index", TypeSymbol.IntegerSym)
  ),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value), Value.IntegerValue(index)) =>
        validateIndex("stringRightFromIndex", index, 0, value.length).map { _ =>
          Value.StringValue(value.drop(index))
        }

      case other => stringExpected("stringRightFromIndex", "one string argument and one integer argument", other)
    }
  }
)

private[StdLib] def StdStringCharAt = initStdFunction(
  "stringCharAt",
  List(
    ("value", TypeSymbol.StringSym),
    ("index", TypeSymbol.IntegerSym)
  ),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value), Value.IntegerValue(index)) =>
        validateIndex("stringCharAt", index, 0, value.length - 1).map { _ =>
          Value.StringValue(value.charAt(index).toString)
        }

      case other => stringExpected("stringCharAt", "one string argument and one integer argument", other)
    }
  }
)

private[StdLib] def StdStringSetCharAt = initStdFunction(
  "stringSetCharAt",
  List(
    ("value", TypeSymbol.StringSym),
    ("index", TypeSymbol.IntegerSym),
    ("charValue", TypeSymbol.StringSym)
  ),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value), Value.IntegerValue(index), Value.StringValue(charValue)) =>
        if charValue.length != 1 then
          Left(StdLibCallErrMsg(
            s"Built-in function 'stringSetCharAt' expected charValue to contain exactly one character, but got: '$charValue'"
          ))
        else
          validateIndex("stringSetCharAt", index, 0, value.length - 1).map { _ =>
            Value.StringValue(value.updated(index, charValue.head))
          }

      case other => stringExpected("stringSetCharAt", "one string, one integer and one string argument", other)
    }
  }
)

private[StdLib] def StdStringRepeat = initStdFunction(
  "stringRepeat",
  List(
    ("value", TypeSymbol.StringSym),
    ("count", TypeSymbol.IntegerSym)
  ),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value), Value.IntegerValue(count)) =>
        if count < 0 then
          Left(StdLibCallErrMsg(
            s"Built-in function 'stringRepeat' expected non-negative count, but got: $count"
          ))
        else
          Right(Value.StringValue(value.repeat(count)))

      case other => stringExpected("stringRepeat", "one string argument and one integer argument", other)
    }
  }
)

private[StdLib] def StdStringFromAsciiCode = initStdFunction(
  "stringFromAsciiCode",
  List(("code", TypeSymbol.IntegerSym)),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.IntegerValue(code)) =>
        if code < 0 || code > 255 then
          Left(StdLibCallErrMsg(
            s"Built-in function 'stringFromAsciiCode' expected code from 0 to 255, but got: $code"
          ))
        else
          Right(Value.StringValue(code.toChar.toString))

      case other => stringExpected("stringFromAsciiCode", "one integer argument", other)
    }
  }
)

private[StdLib] def StdAsciiCodeFromString = initStdFunction(
  "asciiCodeFromString",
  List(("value", TypeSymbol.StringSym)),
  TypeSymbol.IntegerSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value)) =>
        if value.length != 1 then
          Left(StdLibCallErrMsg(
            s"Built-in function 'asciiCodeFromString' expected string with exactly one character, but got: '$value'"
          ))
        else
          Right(Value.IntegerValue(value.head.toInt))

      case other => stringExpected("asciiCodeFromString", "one string argument", other)
    }
  }
)
private[StdLib] def StdStringConcat = initStdFunction(
  "stringConcat",
  List(
    ("left", TypeSymbol.StringSym),
    ("right", TypeSymbol.StringSym)
  ),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(left), Value.StringValue(right)) =>
        Right(Value.StringValue(left + right))

      case other =>
        stringExpected("stringConcat", "two string arguments", other)
    }
  }
)

private[StdLib] def StdStringSubstring = initStdFunction(
  "stringSubstring",
  List(
    ("value", TypeSymbol.StringSym),
    ("start", TypeSymbol.IntegerSym),
    ("count", TypeSymbol.IntegerSym)
  ),
  TypeSymbol.StringSym,
  (actualParamValues, io, callLoc) => {
    actualParamValues match {
      case List(Value.StringValue(value), Value.IntegerValue(start), Value.IntegerValue(count)) =>
        if count < 0 then
          Left(StdLibCallErrMsg(
            s"Built-in function 'stringSubstring' expected non-negative count, but got: $count"
          ))
        else if start < 0 || start > value.length then
          Left(StdLibCallErrMsg(
            s"Built-in function 'stringSubstring' got invalid start index: $start. Expected index from 0 to ${value.length}."
          ))
        else if start + count > value.length then
          Left(StdLibCallErrMsg(
            s"Built-in function 'stringSubstring' got invalid count: $count. Start index is $start, string length is ${value.length}."
          ))
        else
          Right(Value.StringValue(value.substring(start, start + count)))

      case other =>
        stringExpected("stringSubstring", "one string argument and two integer arguments", other)
    }
  }
)