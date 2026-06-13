package com.myway.codinggame.solo.difficult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/zergling-rush
class ZerglingRushTest extends AnyFunSuite with Matchers {

  import ZerglingRush._

  test(" rush") {
    val in = List("5 5", ".....", ".....", "..B..", ".....", ".....")
    solve(in) shouldBe List(".....", ".zzz.", ".zBz.", ".zzz.", ".....")
  }

  test(" rush 2") {
    val in = List("8 5", "...#####", "...#...#", "..B..B.#", "...#...#", "...#####")

    solve(in) shouldBe List("...#####", ".zz#...#", ".zB..B.#", ".zz#...#", "...#####")

  }

  test(" bug") {
    val in = List(
      "15 13",
      ".....#####.....",
      "....##...##....",
      "...##.....##...",
      "..##.......##..",
      ".##..BBB....##.",
      "##...BBB.....##",
      "#.........BBB.#",
      "##.BBB....BBB.#",
      ".##BBB...BB..##",
      "##....BB.BB.##.",
      "#.....#....##..",
      ".....###..##...",
      "....##.####...."
    )

    val out = solve(in)

    val expected = List(
      ".....#####.....",
      "....##...##....",
      "...##.....##...",
      "..##.......##..",
      ".##..BBB....##.",
      "##...BBB.....##",
      "#.........BBB.#",
      "##.BBB....BBB.#",
      ".##BBB...BB..##",
      "##zzzzBB.BB.##.",
      "#....z#....##..",
      ".....###..##...",
      "....##.####...."
    )

    out.size shouldBe expected.size
    List.range(0,expected.size).foreach(
      i => s"$i ${out(i)}" shouldBe s"$i ${expected(i)}"
    )
    out shouldBe expected

  }

}
