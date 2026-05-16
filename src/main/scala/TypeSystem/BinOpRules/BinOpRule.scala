package TypeSystem.BinOpRules

import TypeSystem.{BinOp, TypeSpec}

final case class BinOpRule(
                            op: BinOp,
                            rightType: TypeSpec,
                            returnType: TypeSpec
                          )