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
    val in = List("7,1", "11,1", "11,7", "9,7")
    // act
    val out = Point.fromList(in)
    // assert
    out shouldBe List(Point(7, 1), Point(11, 1), Point(11, 7), Point(9, 7))
  }

  test(" parse") {
    // arrange
    val grid: List[String] = List(
      "..............",
      ".......#...#..",
      "..............",
      "..#....#......",
      "..............",
      "..#......#....",
      "..............",
      ".........#.#..",
      ".............."
    )

    // act
    val map = Point.parse(grid)
    // assert
    map.getOrElse(Point(7, 1), '?') shouldBe '#'
  }

  test(" display") {
    // arrange
    def p(x: Int, y: Int): Point = Point(x, y)

    val map = Map(
      p(0, 0) -> '.',
      p(1, 0) -> '.',
      p(2, 0) -> '.',
      p(0, 1) -> '.',
      p(1, 1) -> '#',
      p(2, 1) -> '.',
      p(0, 2) -> 'O',
      p(1, 2) -> 'c',
      p(2, 2) -> '.'
    )
    // act
    val out = Point.display(map)
    // assert
    out shouldBe List(
      "...",
      ".#.",
      "Oc."
    )
  }


  test(" find") {
    // arrange
    val grid: List[String] = List(
      "...........X..",
      ".......#...#..",
      "..............",
      "..X....#......",
      "..............",
      "..#......#....",
      "..............",
      ".........#.#..",
      ".............."
    )

    val map = Point.parse(grid)
    // act
    val all = Point.findAll(map)('X')
    // assert
    all.toSet shouldBe Set(Point(11,0), Point(2,3))
  }
}
