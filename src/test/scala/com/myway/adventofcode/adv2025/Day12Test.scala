package com.myway.adventofcode.adv2025

import com.myway.adventofcode.adv2025.Day12.{parseGrid, Grid}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day12Test extends AnyFunSuite with Matchers {

  val in = List(
    "0:",
    "###",
    "##.",
    "##.",
    "",
    "1:",
    "###",
    "##.",
    ".##",
    "",
    "2:",
    ".##",
    "###",
    "##.",
    "",
    "3:",
    "##.",
    "###",
    "##.",
    "",
    "4:",
    "###",
    "#..",
    "###",
    "",
    "5:",
    "###",
    ".#.",
    "###",
    "",
    "4x4: 0 0 0 0 2 0",
    "12x5: 1 0 1 0 2 2",
    "12x5: 1 0 1 0 3 2"
  )

  test("parseGrid") {
    parseGrid("4x4: 0 13 0 0 2 0") shouldBe Grid(4, 4, List(0, 13, 0, 0, 2, 0))
  }

  test("year:25 day:12 case 1") {
    // arrange
    // act
    val out = Day12.solve1(in)
    // assert
   // out shouldBe 2L
  }

  test("year:25 day:12 case 2") {
    // arrange
    // act
    val out = Day12.solve2(in)
    // assert
    out shouldBe 0L
  }

}
