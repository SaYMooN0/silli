package Parser

class ParserSuite extends munit.FunSuite {

//  test("parse minimal valid program") {
//    val source =
//      """program Main;
//        |begin
//        |end.
//        |""".stripMargin
//
//    val result = constructAst(source)
//
//    assert(result.isRight)
//
//    val ast = result.toOption.get
//    assertEquals(ast.programName._1.value, "Main")
//    assertEquals(ast.block.varDecls.length, 0)
//    assertEquals(ast.block.procDecls.length, 0)
//    assertEquals(ast.block.compoundStmt.stmts.length, 0)
//  }
//
//  test("parse procedure call with params") {
//    val source =
//      """program Main;
//        |
//        |procedure Alpha(a : integer; b : integer);
//        |begin
//        |end;
//        |
//        |begin
//        |  Alpha(1 + 2, 3);
//        |end.
//        |""".stripMargin
//
//    val result = constructAst(source)
//
//    assert(result.isRight)
//  }
}