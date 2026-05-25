package Interpreter

import Parser.Ident
import SemanticAnalyzer.{ProcDeclBoundAstNode, ProcedureId}
import TypeSystem.BuiltInType

import scala.collection.mutable

final class CallStack private(
                               private val stack: mutable.Stack[ActivationRecord]
                             ) {

}

object CallStack {
  def init(programName: Ident): CallStack = {
    val ar = ActivationRecord(
      name = programName.value,
      t = ArType.Program,
      nestingLevel = 1,
      staticLink = None,
      members = Map(),
      procedures = Map()
    );
    val stack = mutable.Stack[ActivationRecord]()
    stack.push(ar)
    CallStack(stack)
  }
}

final case class ActivationRecord(
                                   name: String,
                                   t: ArType,
                                   nestingLevel: Int,
                                   staticLink: Option[ActivationRecord],
                                   members: Map[Ident, Value],
                                   procedures: Map[ProcedureId, ProcedureClosure]
                                 );

enum ArType {
  case Program;
  case Procedure;
}

enum Value(t: BuiltInType) {
  case RealValue(v: Double) extends Value(BuiltInType.RealT)
  case IntegerValue(v: Int) extends Value(BuiltInType.IntegerT)
  case StringValue(v: String) extends Value(BuiltInType.StringT)
  case BooleanValue(v: Boolean) extends Value(BuiltInType.BooleanT)
}


final case class ProcedureClosure(declarationNode: ProcDeclBoundAstNode, definingAr: ActivationRecord); 
