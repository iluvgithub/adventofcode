package com.myway.adventofcode.tools.linear

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UndeterminedSolverTest extends AnyFunSuite with Matchers {

  test("undetermined") {
    // arrange
    val A = Array(
      Array(1.0, 1.0, 0.0),
      Array(1.0, 0.0, 1.0)
    )
    val b = Array(1.0, 2.0)
    // act
    val x = UndeterminedSolver.solve(A, b)
    // assert
    x shouldBe List(2.0, -1.0, 0.0)
  }

}
