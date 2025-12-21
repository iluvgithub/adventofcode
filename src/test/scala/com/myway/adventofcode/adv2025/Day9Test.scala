package com.myway.adventofcode.adv2025

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day9Test extends AnyFunSuite with Matchers {

  val in = List(
    "7,1",
    "11,1",
    "11,7",
    "9,7",
    "9,5",
    "2,5",
    "2,3",
    "7,3")

  test("year:25 day:9 case 1") {
    // arrange
    // act
    val out = Day9.solve1(in)
    // assert
    out shouldBe 50L
  }


  test("year:25 day:9 case 2") {
    // arrange
    // act
    val out = Day9.solve2(in)
    // assert
    out shouldBe 24L
  }

}
