package com.myway.adventofcode.tools.linear

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LeastSquareTest extends AnyFunSuite with Matchers {

  test("solve") {
    // arrange
    val A = Array(
      Array(0.0, 1.0),
      Array(1.0, 1.0),
      Array(2.0, 1.0)
    )
    val b = Array(0.0, 1.0, 3.0)
    // act
    val x = LeastSquare.leastSquaresSolve(A, b)
    // assert
    x shouldBe Array(1.5, -0.1666666666666668)
  }


}
