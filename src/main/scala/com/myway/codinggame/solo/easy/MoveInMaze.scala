package com.myway.codinggame.solo.easy

object MoveInMaze {

  case class Point(x: Int, y: Int) {
    def neighbors: List[Point] = Point(x - 1, y) :: Point(x + 1, y) ::
      Point(x, y - 1) :: Point(x, y + 1) :: Nil
  }

  object Point {

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

  private def find(m: Map[Point, Char]): Char => Option[Point] = c =>
    m.toList.find { case (p, v) => v.equals(c) }.map(_._1)

  def solve(l: List[String]): List[String] = {
    val pts: Map[Point, Char] = Point.parse(l.tail)
    val start                 = find(pts)('S').get
    Point.display(evolve(pts - start + (start -> '0'), Map(start -> '0')))
  }

  def evolve(pts: Map[Point, Char], front: Map[Point, Char]): Map[Point, Char] = {
    val v0    = front.values.max
    val nextV = next(v0)
    val newFront: Map[Point, Char] = front.toList
      .filter { case (_, v) => v == v0 }
      .map(_._1)
      .flatMap(_.neighbors)
      .toSet
      .filter(p => pts.getOrElse(p, '?') == '.')
      .map(p => p -> nextV)
      .toMap
    if (newFront.isEmpty) pts
    else {
      val newPoints =
        newFront.toList.foldLeft(pts)((acc, pair) => acc - pair._1 + (pair._1 -> pair._2))
      evolve(newPoints, newFront)
    }
  }

  def next(c: Char): Char =
    if (c == '9') 'A'
    else (c.toInt + 1).toChar

}
