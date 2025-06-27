package com.myway.adventofcode.adv2024

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day1Test extends AnyFunSuite with Matchers {

  val in = List(
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3")


  test("case 1") {
    // arrange
    // act
    val out = Day1.solve1(in)
    // assert
    out shouldBe "11"
  }


  test("case 2") {
    // arrange
    // act
    val out = Day1.solve2(in)
    // assert
    out shouldBe "31"
  }

}
