package com.myway.adventofcode.tools.point

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PointTest extends AnyFunSuite with Matchers {

  test(" move") {
    // arrange
    val p = Point(1, 1)
    // act
    val n = North.move(p)
    val s = South.move(p)
    val e = East.move(p)
    val w = West.move(p)
    // assert
    n shouldBe Point(1, 0)
    s shouldBe Point(1, 2)
    e shouldBe Point(2, 1)
    w shouldBe Point(0, 1)
  }

  test(" from List") {
    // arrange

    val in = List(
      "7,1",
      "11,1",
      "11,7",
      "9,7")
    // act
    val out = Point.fromList(in)
    // assert
    out shouldBe List(Point(7, 1), Point(11, 1), Point(11, 7), Point(9, 7))
  }
}
