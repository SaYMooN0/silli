package TypeSystem.BinOpRules

import TypeSystem.{BinOp, BuiltInType}

final case class BinOpRule(
                            op: BinOp,
                            rightType: BuiltInType,
                            returnType: BuiltInType
                          )