package com.myway.adventofcode.tools.linear

import com.myway.adventofcode.tools.linear.LinearSolver.solve
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LinearSolverTest extends AnyFunSuite with Matchers {

  test("solve 3x3") {
    // arrange
    val A: Array[Array[Double]] = Array(
      Array(2.0, 1.0, -1.0),
      Array(-3.0, -1.0, 2.0),
      Array(-2.0, 1.0, 2.0)
    )
    val b1: Array[Double] = Array(2.0, -2.0, 1.0)
    val b2: Array[Double] = Array(-3.0, 7.0, 10.0)
    // act
    val solution1 = solve(A, b1)
    val solution2 = solve(A, b2)
    // assert
    solution1.map(Math.round) shouldBe List(1, 1, 1)
    solution2.map(Math.round) shouldBe List(-1, 2, 3)
  }

}
