package com.myway.adventofcode.tools.point

case class Point(x: Int, y: Int) {

  def allFour: List[Point] = Direction.allFour.map(_.move(this))

}


object Point {

  def fromList(l: List[String]): List[Point] = l.map(from)

  def from(s: String): Point = {
    val pair = s.split(",")
    Point(pair(0).toInt, pair(1).toInt)
  }

  def parse(grid: List[String]): Map[Point, Char] = (for {
    (row, y) <- grid.zipWithIndex
    (char, x) <- row.zipWithIndex
  } yield Point(x, y) -> char).toMap
}
