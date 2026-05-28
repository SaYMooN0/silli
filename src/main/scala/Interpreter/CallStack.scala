package Interpreter

import Parser.Ident
import SemanticAnalyzer.{ProcDeclBoundAstNode, ProcedureId}
import TypeSystem.{BuiltInType, Value}

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




final case class ProcedureClosure(declarationNode: ProcDeclBoundAstNode, definingAr: ActivationRecord); 
