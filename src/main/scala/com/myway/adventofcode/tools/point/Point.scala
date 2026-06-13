package com.myway.adventofcode.tools.point

case class Point(x: Int, y: Int) {

  // def allFour: List[Point] = Direction.allFour.map(_.move(this))
  def allFour: List[Point] = List(
    Point(x - 1, y),
    Point(x + 1, y),
    Point(x, y + 1),
    Point(x, y - 1)
  )
}

object Point {

  def find(m: Map[Point, Char]): Char => Option[Point] = c =>
    m.toList.find { case (_, v) => v.equals(c) }.map(_._1)

  def findAll(m:Map[Point,Char]):Char=>List[Point] =  c =>
    m.toList.filter { case (_, v) => v.equals(c) }.map(_._1)

  def fromList(l: List[String]): List[Point] = l.map(from)

  def from(s: String): Point = {
    val pair = s.split(",")
    Point(pair(0).toInt, pair(1).toInt)
  }

  def parse(grid: List[String]): Map[Point, Char] = (for {
    (row, y)  <- grid.zipWithIndex
    (char, x) <- row.zipWithIndex
  } yield Point(x, y) -> char).toMap

  def display(map0: Map[Point, Char]): List[String] = if (map0.isEmpty) Nil
  else {

    val xs = map0.keys.map(_.x)
    val ys = map0.keys.map(_.y)

    val minX = xs.min
    val maxX = xs.max
    val minY = ys.min
    val maxY = ys.max
    List
      .range(minY, maxY + 1)
      .map(y => List.range(minX, maxX + 1).map(x => map0.getOrElse(Point(x, y), '?')).mkString)
  }

}
