package com.myway.codinggame.solo.easy

// https://www.codingame.com/ide/puzzle/island-escape
object IslandEscape {

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

  def solve(l: List[String]): String = {

    val n                     = l.head.toInt
    val in: List[String]      = l.tail.map(s => s.split(" ").map(_.toInt).toList.mkString)
    val map: Map[Point, Char] = Point.parse(in)
    val init                  = Point(n / 2, n / 2)
    val out: Set[Point]       = go(map, Set(init))
    val b = out.toList.exists(p => p.x <= 0 || p.y <= 0 || p.x >= (n - 1) || p.y >= (n - 1))
    if (b) "yes" else "no"
  }

  def go(map: Map[Point, Char], set: Set[Point]): Set[Point] = {
    val newSet = set.flatMap(x =>
      (x :: x.allFour.filter(p =>
        map.contains(p) && Math.abs(map(p) - map(x)) <= 1
      )).toSet
    )
    if (newSet.size <= set.size) set else go(map, newSet)

  }

}
