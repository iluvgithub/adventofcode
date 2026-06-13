package com.myway.codinggame.solo.easy

// https://www.codingame.com/ide/puzzle/the-lost-child-episode-1
object LostChildEpOne {

  import scala.collection.mutable

  case class Point(x: Int, y: Int) {

    def allFour: List[Point] = List(
      this.copy(x = this.x - 1),
      this.copy(x = this.x + 1),
      this.copy(y = this.y - 1),
      this.copy(y = this.y + 1)
    )

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

  trait AstarGraph[A] {
    def next(node: A): List[A]
  }

  object Astar {

    def find[A](
      g: AstarGraph[A],
      cost: A => A => Long,
      from: A,
      to: A
    ): List[A] = {

      val gScore   = mutable.Map[A, Long]().withDefaultValue(Long.MaxValue)
      val fScore   = mutable.Map[A, Long]().withDefaultValue(Long.MaxValue)
      val cameFrom = mutable.Map[A, A]()

      gScore(from) = 0L
      fScore(from) = cost(from)(to) // using cost as heuristic

      implicit val ordering: Ordering[(Long, A)] = Ordering.by(-_._1) // min-heap by fScore
      val openSet = mutable.PriorityQueue[(Long, A)]((fScore(from), from))

      val closed = mutable.Set[A]()

      while (openSet.nonEmpty) {
        val (_, current) = openSet.dequeue()

        if (current == to) { // uses standard == (works fine for case classes, etc.)
          def reconstruct(node: A): List[A] =
            cameFrom.get(node) match {
              case None       => List(node)
              case Some(prev) => reconstruct(prev) :+ node
            }

          return reconstruct(to)
        }

        closed += current

        for (neighbor <- g.next(current))
          if (!closed.contains(neighbor)) {
            val tentativeG = gScore(current) + cost(current)(neighbor)

            if (tentativeG < gScore(neighbor)) {
              cameFrom(neighbor) = current
              gScore(neighbor) = tentativeG
              fScore(neighbor) = tentativeG + cost(neighbor)(to)

              openSet += ((fScore(neighbor), neighbor)) // duplicate entries ok
            }
          }
      }

      Nil // no path found
    }
  }

  def solve(l: List[String]): String = {
    val pts: Map[Point, Char] = Point.parse(l)
    s"${solvePts(pts)}km"
  }

  def solvePts(pts: Map[Point, Char]): Long = {
    val gr = new AstarGraph[Point] {

      override def next(node: Point): List[Point] = node.allFour.filter(p =>
        pts.getOrElse(p, '?').equals('.') || pts.getOrElse(p, '?').equals('M')
      )
    }
    val child  = Point.find(pts)('C').get
    val mother = Point.find(pts)('M').get

    val cost: Point => Point => Long = _ => _ => 1L

    val path = Astar.find(gr, cost, child, mother)
    (path.size - 1) * 10
  }

}
