package com.myway.codinggame.solo.easy
// https://www.codingame.com/ide/puzzle/where-was-this-knight-before
object KnightBefore {

  case class Point(x: Int, y: Int) {

    def diff(that: Point) = Point(this.x - that.x, this.y - that.y)
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

  def purge(dico: List[Char]): Map[Point, Char] => Map[Point, Char] = map =>
    map.keySet.toList.foldLeft(Map[Point, Char]())(
      (acc, k) => {
        val v = if(dico.contains(map(k).toLower)) map(k) else '.'
        acc + (k -> v)
      }
    )
  def solve(l: List[String]): List[String] = {
    val dico                     = l.head.toList.map(_.toLower)
    val before: Map[Point, Char] = purge(dico)(Point.parse(l.tail.take(8)))
    val after: Map[Point, Char]  = purge(dico)(Point.parse(l.drop(9)))

    val diff = after.keys.filterNot(k => before(k) == after(k)).toList
    val p1   = diff.head
    val p2   = diff.tail.head
    val pt   = p1.diff(p2)
    val X    = Math.abs(pt.x)
    val Y    = Math.abs(pt.y)
    val sp1  = s"${xToLetter(p1.x)}${8 - p1.y}"
    val sp2  = s"${xToLetter(p2.x)}${8 - p2.y}"
    val s    = if ((X == 2 && Y == 1) || (X == 1 && Y == 2)) "Knight" else "Other"

    val pref = if (after(p1) == '.') {
      val sep = if (before(p2) == '.') "-" else "x"
      s"$sp1$sep$sp2"
    } else {
      val sep = if (before(p1) == '.') "-" else "x"
      s"$sp2$sep$sp1"
    }
    pref :: s :: Nil
  }

  def xToLetter(i: Int): Char = ('a' + i).toChar
}
