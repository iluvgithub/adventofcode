package com.myway.cats.tagless.expr.`final`

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ArithmeticTest extends AnyFunSuite with Matchers {

  // programs
  def onePlusTwo[Expr](arithmetic: Arithmetic[Expr]): Expr =
    arithmetic + (arithmetic.literal(1.0), arithmetic.literal(2.0))

  def onePlusTwoTimesThree[Expr](arithmetic: Arithmetic[Expr]): Expr =
    arithmetic * (
      onePlusTwo(arithmetic),
      arithmetic.literal(3)
    )


  def oneMinusTwo[Expr](arithmetic: Arithmetic[Expr]): Expr =
    arithmetic - (arithmetic.literal(1.0), arithmetic.literal(2.0))

  def oneDivTwo[Expr](arithmetic: Arithmetic[Expr]): Expr =
    arithmetic / (arithmetic.literal(1.0), arithmetic.literal(2.0))

  test("eval double") {
    onePlusTwo(DoubleArithmetic) shouldBe 3.0
    onePlusTwoTimesThree(DoubleArithmetic) shouldBe 9.0
    oneMinusTwo(DoubleArithmetic) shouldBe -1.0
    oneDivTwo(DoubleArithmetic) shouldBe 0.5
  }

  test("print double") {
    onePlusTwo(PrintArithmetic) shouldBe "(1.0 + 2.0)"
    onePlusTwoTimesThree(PrintArithmetic) shouldBe "((1.0 + 2.0) * 3.0)"
    oneMinusTwo(PrintArithmetic) shouldBe "(1.0 - 2.0)"
    oneDivTwo(PrintArithmetic) shouldBe "(1.0 / 2.0)"
  }


}
