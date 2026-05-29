package Interpreter

import Parser.Ident
import SemanticAnalyzer.{ProcDeclBoundAstNode, ProcedureId}
import TypeSystem.{BuiltInType, Value}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

enum CallstackErr {
  case EmptyCallstack
  case VariableNotDeclared(ident: Ident)
}

final class Callstack private(private val stack: mutable.Stack[ActivationRecord]) {
  def init(programName: Ident): Callstack = {
    val ar = ActivationRecord(
      name = programName.value,
      t = ArType.Program,
      nestingLevel = 1,
      staticLink = None,
      members = mutable.Map(),
      procedures = mutable.Map()
    );
    val stack = mutable.Stack[ActivationRecord]()
    stack.push(ar)
    Callstack(stack)
  }

  private def currentAr: Either[CallstackErr, ActivationRecord] = {
    stack.headOption match {
      case Some(ar) => Right(ar)
      case None => Left(CallstackErr.EmptyCallstack)
    }
  }
  def declareVariable(ident: Ident): Either[CallstackErr, Unit] = {
    currentAr.map { ar => ar.members.update(ident, ValueOrUndefined.Undefined)
    }
  }

  def setVariable(ident: Ident, value: Value): Either[CallstackErr, Unit] = {
    currentAr.flatMap { ar =>
      ar.tryFindMemberOwner(ident) match {
        case None => Left(CallstackErr.VariableNotDeclared(ident))
        case Some(ownerAr) =>
          ownerAr.members.update(ident, ValueOrUndefined.Value(value))
          Right(())
      }
    }
  }

  def getVariable(ident: Ident): Either[CallstackErr, ValueOrUndefined] = {
    currentAr.flatMap { ar =>
      ar.tryFindMember(ident) match {
        case Some(valueOrUndefined) => Right(valueOrUndefined)
        case None => Left(CallstackErr.VariableNotDeclared(ident))
      }
    }
  }


}

private final case class ActivationRecord(
                                           name: String,
                                           t: ArType,
                                           nestingLevel: Int,
                                           staticLink: Option[ActivationRecord],
                                           members: mutable.Map[Ident, ValueOrUndefined],
                                           procedures: mutable.Map[ProcedureId, ProcedureClosure]
                                         ) {
  def tryFindMemberOwner(ident: Ident): Option[ActivationRecord] = {
    if members.contains(ident) then Some(this)
    else this.staticLink.flatMap(_.tryFindMemberOwner(ident))
  }

  def tryFindMember(ident: Ident): Option[ValueOrUndefined] =
    members
      .get(ident)
      .orElse(this.staticLink.flatMap(_.tryFindMember(ident)))
}

enum ArType {
  case Program;
  case Procedure;
}


final case class ProcedureClosure(declarationNode: ProcDeclBoundAstNode, definingAr: ActivationRecord); 
