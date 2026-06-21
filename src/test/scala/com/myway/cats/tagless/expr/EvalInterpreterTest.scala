package com.myway.cats.tagless.expr

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EvalInterpreterTest extends AnyFunSuite with Matchers {

  val onePlusTwo = Add(Literal(1), Literal(2))
  val onePlusTwoTimesThree = Mul(Add(Literal(1), Literal(2)), Literal(3))
  val oneMinusTwo = Sub(Literal(1), Literal(2))
  val oneDivTwo = Div(Literal(1), Literal(2))

  test("eval double") {
    DoubleInterpreter.eval(onePlusTwo) shouldBe 3
    DoubleInterpreter.eval(onePlusTwoTimesThree) shouldBe 9
    DoubleInterpreter.eval(oneMinusTwo) shouldBe -1
    DoubleInterpreter.eval(oneDivTwo) shouldBe 0.5
  }


  test("print") {
    PrintInterpreter.print(onePlusTwo) shouldBe "(1.0 + 2.0)"
    PrintInterpreter.print(onePlusTwoTimesThree) shouldBe "((1.0 + 2.0) * 3.0)"
    PrintInterpreter.print(oneMinusTwo) shouldBe "(1.0 - 2.0)"
    PrintInterpreter.print(oneDivTwo) shouldBe "(1.0 / 2.0)"
  }

}
