package SemanticAnalyzer

sealed trait SymbolTable {
  def dict: Map[String, SemanticSymbol]

  def level: Int

  def name: String

  protected def parentOpt: Option[SymbolTable]

  def lookupLocal(name: String): Option[SemanticSymbol] =
    dict.get(name)

  def lookup(name: String): Option[SemanticSymbol] =
    dict.get(name).orElse(parentOpt.flatMap(_.lookup(name)))
}

final class BuiltinsScopeSymbolTable private(val dict: Map[String, SemanticSymbol]) extends SymbolTable {

  override val level: Int = 0
  override val name: String = "builtins"

  override protected def parentOpt: Option[SymbolTable] = None
}

private object BuiltinsScopeSymbolTable {
  def init: BuiltinsScopeSymbolTable = new BuiltinsScopeSymbolTable(Map(
    TypeSymbol.IntegerSym.spec.name -> TypeSymbol.IntegerSym,
    TypeSymbol.RealSym.spec.name -> TypeSymbol.RealSym,
    TypeSymbol.BooleanSym.spec.name -> TypeSymbol.BooleanSym,
    TypeSymbol.StringSym.spec.name -> TypeSymbol.StringSym
  ))
}

final class GlobalScopeSymbolTable private(
                                            val dict: Map[String, SemanticSymbol],
                                            val parent: BuiltinsScopeSymbolTable
                                          ) extends SymbolTable {

  override val level: Int = 1
  override val name: String = "global"

  override protected def parentOpt: Option[SymbolTable] = Some(parent)

  def withSymbol(symbolName: String, symbol: SemanticSymbol): GlobalScopeSymbolTable =
    new GlobalScopeSymbolTable(
      dict = dict + (symbolName -> symbol),
      parent = parent
    )

  def createChildScope(name: String): ScopedSymbolTable = ScopedSymbolTable.create(name, this)
}

object GlobalScopeSymbolTable {
  def init: GlobalScopeSymbolTable = new GlobalScopeSymbolTable(Map.empty, BuiltinsScopeSymbolTable.init)
}

final class ScopedSymbolTable private(
                                       val dict: Map[String, SemanticSymbol],
                                       val level: Int,
                                       val name: String,
                                       val parent: GlobalScopeSymbolTable | ScopedSymbolTable
                                     ) extends SymbolTable {

  require(level > 1, "ScopedSymbolTable level must be > 1")
  require(level == parent.level + 1, "ScopedSymbolTable level must be parent's level + 1")

  override protected def parentOpt: Option[SymbolTable] = Some(parent)

  def withSymbol(symbolName: String, symbol: SemanticSymbol): ScopedSymbolTable =
    new ScopedSymbolTable(
      dict = dict + (symbolName -> symbol),
      level = level,
      name = name,
      parent = parent
    )

  def createChildScope(name: String): ScopedSymbolTable = ScopedSymbolTable.create(name, parent = this)
}

private object ScopedSymbolTable {
  def create(name: String, parent: GlobalScopeSymbolTable | ScopedSymbolTable): ScopedSymbolTable =
    new ScopedSymbolTable(
      dict = Map.empty,
      level = parent.level + 1,
      name = name,
      parent = parent
    )
  
}