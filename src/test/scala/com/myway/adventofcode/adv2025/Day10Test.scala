package com.myway.adventofcode.adv2025

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day10Test extends AnyFunSuite with Matchers {

  val in = List(
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
    "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
    "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
  )

  test("year:25 day:10 case 1") {
    // arrange
    // act
    val out = Day10.solve1(in)
    // assert
    out shouldBe 7L
  }

  test("year:25 day:10 case 2") {
    // arrange
    // https://chatgpt.com/c/6949d26f-d40c-8325-ad61-8b4e011bfd75
    // act
    val out = Day10.solve2(in)
    // assert
    out shouldBe 33L
  }

}
