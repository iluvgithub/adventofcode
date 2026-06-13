package com.myway.codinggame.solo.medium

object TiredPacMan {

  trait Direction {
    def move(p: Point): Point
    def turnLeft: Direction
    def opposite: Direction
  }

  object North extends Direction {
    override def move(p: Point): Point = p.copy(y = p.y - 1)

    override def turnLeft: Direction = West

    override def opposite: Direction = South
  }

  object South extends Direction {
    override def move(p: Point): Point = p.copy(y = p.y + 1)

    override def turnLeft: Direction = East

    override def opposite: Direction = North
  }

  object East extends Direction {
    override def move(p: Point): Point = p.copy(x = p.x + 1)

    override def turnLeft: Direction = North

    override def opposite: Direction = West
  }

  object West extends Direction {
    override def move(p: Point): Point = p.copy(x = p.x - 1)

    override def turnLeft: Direction = South

    override def opposite: Direction = East
  }

  object Direction {

    val allFour: List[Direction]  = List(North, East, South, West)
    val diagonal: List[Direction] = allFour.map(_.turnLeft)
  }

  case class Point(x: Int, y: Int) {

    def allFour: List[Point] = Direction.allFour.map(_.move(this))

  }

  object Point {

    def find(m: Map[Point, Char]): Char => Option[Point] = c =>
      m.toList.find { case (_, v) => v.equals(c) }.map(_._1)

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

  val fruits: Map[Char, Long] = Map('*' -> 5L, '.' -> 1L, ')' -> 3L)

  def solve(l: List[String]): Long = {
    val energy                = l.tail.head.toInt
    val map: Map[Point, Char] = Point.parse(l.tail.tail)
    val ghosts: Set[Point] =
      map.keySet.filter(p => map.getOrElse(p, '?') == 'G').flatMap(p => p :: p.allFour)
    val init = Point.find(map)('P').get

    go(map, ghosts, energy, init :: Nil, Map(init -> 0L))
  }

  def go(
    map: Map[Point, Char],
    ghosts: Set[Point],
    energy: Int,
    points: List[Point],
    scores: Map[Point, Long]
  ): Long =
    if (energy <= 0)
      scores.values.sum
    else {
      val newPos = points.flatMap(_.allFour).filter { p =>
        val c = map(p)
        (c == ' ' || fruits.keySet.contains(c)) && !scores.contains(p) && !ghosts.contains(p)
      }
      val newScores = scores ++ newPos.foldLeft(Map[Point, Long]())((acc, kp) =>
        acc + (kp -> fruits.getOrElse(map(kp), 0L))
      )

      go(map, ghosts, energy - 1, newPos, newScores)
    }
}
