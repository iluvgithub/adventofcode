package com.myway.codinggame.solo.easy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/moves-in-maze
class MoveInMazeTest extends AnyFunSuite with Matchers {

  test(" browse") {

    MoveInMaze.solve(
      List(
        "10 5",
        "##########",
        "#S.......#",
        "##.#####.#",
        "##.#.....#",
        "##########")
    ) shouldBe  List(
      "##########",
      "#01234567#",
      "##2#####8#",
      "##3#DCBA9#",
      "##########")
  }

  test(" browse cylinder") {
    val o=  MoveInMaze.solve(
        List(
          "10 5",
          "#.########",
          "#.##..####",
          "..##..#...",
          "####..#S##",
          "#....#####")
    )
    
    o shouldBe  List(
      "#7########",
      "#6##EF####",
      "45##DE#123",
      "####CD#0##",
      "#89AB#####")
  }


}
