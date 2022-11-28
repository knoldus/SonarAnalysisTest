package recfun

class RecFunSuite extends munit.FunSuite {
  import RecFun._

  // ------ balance tests -----------------------------------------------------

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }

  test("balance: list has no braces") {
    assert(balance("I need no braces!".toList))
  }

  test("balance: is unballanced") {
    assert(!balance("(this sentence has no closure".toList))
  }

  test("balance: is unballanced but more than one brace") {
    assert(!balance(")fasdf))Had0".toList))
  }

  // ------ countChange tests -------------------------------------------------

  test("countChange: example given in instructions") {
    assertEquals(countChange(4,List(1,2)), 3)
  }

  test("countChange: sorted CHF") {
    assertEquals(countChange(300,List(5,10,20,50,100,200,500)), 1022)
  }

  test("countChange: no pennies") {
    assertEquals(countChange(301,List(5,10,20,50,100,200,500)), 0)
  }

  test("countChange: unsorted CHF") {
    assertEquals(countChange(300,List(500,5,50,100,20,200,10)), 1022)
  }

  // ------ pascal tests ------------------------------------------------------

  test("pascal: col=0,row=2") {
    assertEquals(pascal(0, 2), 1)
  }

  test("pascal: col=1,row=2") {
    assertEquals(pascal(1, 2), 2)
  }

  test("pascal: col=1,row=3") {
    assertEquals(pascal(1, 3), 3)
  }

  test("pascal: col = 1. row=0. This should return 0 since this is a invalid index pair"){
    assertEquals(pascal(1,0),0)
  }

  test("pascal: col=-1,row=-2. Should return 0"){
    assertEquals(pascal(-1,-2),0)
  }

  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}
