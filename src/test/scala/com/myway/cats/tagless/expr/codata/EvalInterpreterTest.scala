package com.myway.cats.tagless.expr.codata

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EvalInterpreterTest extends AnyFunSuite with Matchers {

  import Expr._

  val onePlusTwo: Expr = literal(1) add literal(2)
  val onePlusTwoTimesThree: Expr = onePlusTwo mult literal(3)
  val oneMinusTwo: Expr = literal(1) sub literal(2)
  val oneDivTwo: Expr = literal(1) div literal(2)

  test("eval double") {
    onePlusTwo.eval shouldBe 3
    onePlusTwoTimesThree.eval shouldBe 9
    oneMinusTwo.eval shouldBe -1
    oneDivTwo.eval shouldBe 0.5

    sin(onePlusTwo).eval shouldBe 0.1411200080598672
  }


  test("print") {
    onePlusTwo.print shouldBe "(1.0 + 2.0)"
    onePlusTwoTimesThree.print shouldBe "((1.0 + 2.0) * 3.0)"
    oneMinusTwo.print shouldBe "(1.0 - 2.0)"
    oneDivTwo.print shouldBe "(1.0 / 2.0)"


    sin(onePlusTwo).print shouldBe "sin((1.0 + 2.0))"

  }
}
