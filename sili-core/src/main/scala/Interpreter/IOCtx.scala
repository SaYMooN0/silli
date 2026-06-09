package Interpreter

trait IOCtx {

  def read(): String;

  def write(value: String): Unit
}
