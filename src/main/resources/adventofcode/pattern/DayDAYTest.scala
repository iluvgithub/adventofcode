package com.myway.adventofcode.adv20YEAR

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DayDAYTest extends AnyFunSuite with Matchers {

  val in = List( )

  test("year:YEAR day:DAY case 1") {
    // arrange
    // act
    val out = DayDAY.solve1(in)
    // assert
    out shouldBe ""
  }


  test("year:YEAR day:DAY case 2") {
    // arrange
    // act
    val out = DayDAY.solve2(in)
    // assert
    out shouldBe ""
  }

}
