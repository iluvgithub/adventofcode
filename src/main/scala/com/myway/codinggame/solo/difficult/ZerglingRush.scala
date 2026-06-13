package com.myway.codinggame.solo.difficult

object ZerglingRush {

  import scala.annotation.tailrec
  case class Point(x: Int, y: Int) {
    def allFour: List[Point] = List(
      Point(x - 1, y),
      Point(x + 1, y),
      Point(x, y + 1),
      Point(x, y - 1)
    )

    def diags: List[Point] = List(
      Point(x - 1, y - 1),
      Point(x - 1, y + 1),
      Point(x + 1, y + 1),
      Point(x + 1, y - 1)
    )

  }

  object Point {

    def find(m: Map[Point, Char]): Char => Option[Point] = c =>
      m.toList.find { case (_, v) => v.equals(c) }.map(_._1)

    def findAll(m: Map[Point, Char]): Char => List[Point] = c =>
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

  def solve(l: List[String]): List[String] = {
    val Array(w, h) = (l.head split " ").filter(_ != "").map(_.toInt)

    val maze: Map[Point, Char] = Point.parse(l.tail)
    val bases: List[Point]     = Point.findAll(maze)('B')
    val allFree = bases
      .flatMap(basis => basis.diags ++ basis.allFour)
      .filter(p => maze.getOrElse(p, '?') == '.')
      .filter(p => canFindExit(maze)(p))

    val out     = allFree.foldLeft(maze)((acc, p) => acc - p + (p -> 'z'))
    val display = Point.display(out)

    display
  }

  def canFindExit(maze: Map[Point, Char]): Point => Boolean = init => {
    val maxX = maze.keySet.map(_.x).max
    val maxY = maze.keySet.map(_.y).max

    val o: Set[Point] = toExit(maze, Set(init))
    if (o.isEmpty) false
    else {
      val xs = o.map(_.x)
      val ys = o.map(_.y)
      xs.min <= 0 || xs.max >= maxX || ys.min <= 0 || ys.max >= maxY
    }
  }

  @tailrec
  private def toExit(
    maze: Map[Point, Char],
    points: Set[Point]
  ): Set[Point] = {
    val newPoints =
      points.flatMap(p => p:: p.allFour.filter(x => maze.getOrElse(x, '?') == '.'))
    if (newPoints.size > points.size) toExit(maze, newPoints) else newPoints
  }
}
