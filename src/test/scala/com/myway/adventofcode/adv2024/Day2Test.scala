package com.myway.adventofcode.adv2024

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day2Test extends AnyFunSuite with Matchers {

  val in = List(
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9")

  test("parse") {
    Day2.parse(in.head) shouldBe List(7 ,6, 4, 2, 1)
  }

  test("year:24 day:2 case 1") {
    // arrange
    // act
    val out = Day2.solve1(in)
    // assert
    out shouldBe 2
  }


  test("year:24 day:2 case 2") {
    // arrange
    // act
    val out = Day2.solve2(in)
    // assert
    out shouldBe 4
  }

}
