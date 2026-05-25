package com.myway.codinggame.solo

object FindConsecutive {

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

  case class State(
    currentPt: Point,
    current: Char,
    map: Map[Point, Char],
    targetMap: Map[Point, Char]
  )

  def solve(l: List[String]): List[String] = {
    val sz                    = l.head.toInt
    val map: Map[Point, Char] = Point.parse(l.tail)
    val points: List[Point]   = map.toList.filter { case (_, v) => v.equals('a') }.map(_._1)

    val mapOut: Map[Point, Char] = points.map(p => solveFrom(p, map)).maxBy(_.size)

    List
      .range(0, sz)
      .map(y => List.range(0, sz).map(x => mapOut.getOrElse(Point(x, y), '-')).mkString)
  }

  def solveFrom(initPoint: Point, map: Map[Point, Char]): Map[Point, Char] = {

    val init = State(initPoint, 'a', map, Map(initPoint -> 'a'))
    val list = List
      .unfold[State, State](init) { st =>
        val currentPoint   = st.currentPt
        val nextChar: Char = if (st.current.equals('z')) 'a' else (st.current + 1).toChar
        val optState: Option[State] = for {
          nextPoint <- currentPoint.neighbors.find(pt => st.map.get(pt).exists(_.equals(nextChar)))
        } yield State(
          nextPoint,
          nextChar,
          st.map - currentPoint,
          st.targetMap + (nextPoint -> nextChar)
        )

        optState.map(s => (s, s))
      }
    if (list.isEmpty) Map() else list.last.targetMap

  }
}
