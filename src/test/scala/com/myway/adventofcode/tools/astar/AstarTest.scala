package com.myway.adventofcode.tools.astar

import com.myway.adventofcode.tools.point.Point
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AstarTest extends AnyFunSuite with Matchers {
  object GridGraph extends AstarGraph[Point] {
    private val width     = 10
    private val height    = 10
    private val obstacles = Set(Point(3, 3), Point(3, 4), Point(3, 5), Point(4, 5), Point(5, 5))

    override def next(current: Point): List[Point] = {
      val candidates = List(
        Point(current.x + 1, current.y),
        Point(current.x - 1, current.y),
        Point(current.x, current.y + 1),
        Point(current.x, current.y - 1)
        // You can add diagonals if desired
      )
      candidates.filter { p =>
        p.x >= 0 && p.x < width &&
        p.y >= 0 && p.y < height &&
        !obstacles.contains(p)
      }
    }

    def manhattanHeuristic(to: Point)(from: Point): Long =
      math.abs(from.x - to.x).toLong + math.abs(from.y - to.y).toLong
  }

  test("A* finds path in empty grid") {
    val path = Astar.find(
      GridGraph,
      GridGraph.manhattanHeuristic,
      Point(0, 0),
      Point(9, 9)
    )

    path.length shouldBe 19 // Manhattan distance 18 + 1 = 19 nodes
  }

  test("A* avoids obstacles") {
    val path = Astar.find(
      GridGraph,
      GridGraph.manhattanHeuristic,
      Point(2, 2),
      Point(6, 6)
    )

    path.contains(Point(3, 5)) shouldBe false
    path.length shouldBe 9
  }

}
