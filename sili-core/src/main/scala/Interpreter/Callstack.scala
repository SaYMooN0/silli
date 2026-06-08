package Interpreter

import Parser.Ident
import SemanticAnalyzer.*
import TypeSystem.{BuiltInType, Value}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

enum CallstackErr {
  case EmptyCallstack
  case VariableNotDeclared(ident: Ident)
  case ProcedureClosureNotFound(procId: ProcedureId)
  case FunctionClosureNotFound(id: FunctionId)
  case ActualParamsCountMismatch(procName: Ident, expected: Int, actual: Int)
}

object Callstack {
  def init(programName: Ident): Callstack = {
    val ar = ActivationRecord(
      name = programName.value,
      t = ArType.Program,
      nestingLevel = 1,
      staticLink = None,
      members = mutable.Map(),
      procedures = mutable.Map(),
      functions = mutable.Map()
    )
    val stack = mutable.Stack[ActivationRecord]()
    stack.push(ar)
    Callstack(stack)
  }
}

final class Callstack private(private val stack: mutable.Stack[ActivationRecord]) {

  private def currentAr: Either[CallstackErr, ActivationRecord] = {
    stack.headOption match {
      case Some(ar) => Right(ar)
      case None     => Left(CallstackErr.EmptyCallstack)
    }
  }

  def declareVariable(ident: Ident): Either[CallstackErr, Unit] = {
    currentAr.map { ar => ar.members.update(ident, ValueOrUndefined.Undefined)
    }
  }

  def setVariable(ident: Ident, value: Value): Either[CallstackErr, Unit] = {
    currentAr.flatMap { ar =>
      ar.tryFindMemberOwner(ident) match {
        case None          => Left(CallstackErr.VariableNotDeclared(ident))
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
        case None                   => Left(CallstackErr.VariableNotDeclared(ident))
      }
    }
  }

  def declareProcedureClosure(procDecl: DeclItemBoundAstNode.ProcDecl): Either[CallstackErr, Unit] = {
    currentAr.map { ar => ar.procedures.update(procDecl.procSym.id, ProcedureClosure(procDecl, ar)) }
  }

  def declareFunctionClosure(funcDecl: DeclItemBoundAstNode.FuncDecl): Either[CallstackErr, Unit] = {
    currentAr.map { ar => ar.functions.update(funcDecl.funcSym.id, FunctionClosure(funcDecl, ar)) }
  }

  private def enterCallableCall[D](
                                    callableName: Ident,
                                    formalParams: List[ParamSymbol],
                                    scopeLevel: Int,
                                    actualParamValues: List[Value],
                                    arType: ArType,
                                    findClosure: ActivationRecord => Option[(D, ActivationRecord)],
                                    closureNotFoundErr: => CallstackErr,
                                    extraMembers: List[(Ident, ValueOrUndefined)] = List.empty
                                  ): Either[CallstackErr, D] = {
    currentAr.flatMap { ar =>
      findClosure(ar) match {
        case None => Left(closureNotFoundErr)

        case Some((declarationNode, definingAr)) =>
          if formalParams.length != actualParamValues.length then {
            Left(CallstackErr.ActualParamsCountMismatch(
              callableName,
              formalParams.length,
              actualParamValues.length
            ))
          } else {
            val paramMembers = formalParams
              .zip(actualParamValues)
              .map { case (formalParam, actualValue) =>
                formalParam.name -> ValueOrUndefined.Value(actualValue)
              }

            val newMembers = mutable.Map.from(
              paramMembers ++ extraMembers
            )

            val newAr = ActivationRecord(
              name = callableName.value,
              t = arType,
              nestingLevel = scopeLevel + 1,
              staticLink = Some(definingAr),
              members = newMembers,
              procedures = mutable.Map(),
              functions = mutable.Map()
            )

            stack.push(newAr)

            Right(declarationNode)
          }
      }
    }
  }

  def enterProcCall(procSym: ProcSymbol, actualParamValues: List[Value]): Either[CallstackErr, DeclItemBoundAstNode.ProcDecl] = {
    enterCallableCall(
      procSym.procName, procSym.formalParams, procSym.scopeLevel, actualParamValues, ArType.Procedure,
      findClosure = ar =>
        ar.tryFindProcedureClosure(procSym.id).map { closure =>
          (closure.declarationNode, closure.definingAr)
        },
      closureNotFoundErr = CallstackErr.ProcedureClosureNotFound(procSym.id)
    )
  }

  def enterFuncCall(funcSym: FuncSymbol, actualParamValues: List[Value]): Either[CallstackErr, DeclItemBoundAstNode.FuncDecl] = {
    enterCallableCall(
      funcSym.funcName, funcSym.formalParams, funcSym.scopeLevel, actualParamValues, ArType.Function,
      findClosure = ar =>
        ar.tryFindFunctionClosure(funcSym.id).map { closure =>
          (closure.declarationNode, closure.definingAr)
        },
      closureNotFoundErr = CallstackErr.FunctionClosureNotFound(funcSym.id),
      extraMembers = List(FuncResultSymbol.ResultIdent -> ValueOrUndefined.Undefined)
    )
  }

  def leaveCall(): Either[CallstackErr, Unit] = {
    currentAr.map { _ => stack.pop() }
  }
}

private final case class ActivationRecord(
                                           name: String,
                                           t: ArType,
                                           nestingLevel: Int,
                                           staticLink: Option[ActivationRecord],
                                           members: mutable.Map[Ident, ValueOrUndefined],
                                           procedures: mutable.Map[ProcedureId, ProcedureClosure],
                                           functions: mutable.Map[FunctionId, FunctionClosure]
                                         ) {
  def tryFindMemberOwner(ident: Ident): Option[ActivationRecord] = {
    if members.contains(ident) then Some(this)
    else this.staticLink.flatMap(_.tryFindMemberOwner(ident))
  }

  def tryFindMember(ident: Ident): Option[ValueOrUndefined] =
    members
      .get(ident)
      .orElse(this.staticLink.flatMap(_.tryFindMember(ident)))

  def tryFindProcedureClosure(procId: ProcedureId): Option[ProcedureClosure] =
    procedures
      .get(procId)
      .orElse(staticLink.flatMap(_.tryFindProcedureClosure(procId)))

  def tryFindFunctionClosure(funcId: FunctionId): Option[FunctionClosure] =
    functions
      .get(funcId)
      .orElse(staticLink.flatMap(_.tryFindFunctionClosure(funcId)))

}

enum ArType {
  case Program
  case Procedure
  case Function
}


final case class ProcedureClosure(declarationNode: DeclItemBoundAstNode.ProcDecl, definingAr: ActivationRecord);

final case class FunctionClosure(declarationNode: DeclItemBoundAstNode.FuncDecl, definingAr: ActivationRecord); 
