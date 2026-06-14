package com.myway.adventofcode.tools.astar.heuristics

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AstarHeuristicTest extends AnyFunSuite with Matchers {

  case class Pos(x: Int, y: Int)

  class GridGraph(width: Int, height: Int)
    extends AstarGraph[Pos] {

    override def next(node: Pos): List[Pos] =
      List(
        Pos(node.x + 1, node.y),
        Pos(node.x - 1, node.y),
        Pos(node.x, node.y + 1),
        Pos(node.x, node.y - 1)
      ).filter(p =>
        p.x >= 0 &&
          p.x < width &&
          p.y >= 0 &&
          p.y < height
      )
  }

  val cost: Pos => Pos => Double =
    _ => _ => 1.0

  val heuristic: Pos => Pos => Double =
    from => to =>
      math.abs(from.x - to.x) +
        math.abs(from.y - to.y)

  test("find shortest path on a grid") {

    val path =
      AstarHeuristic.find(
        new GridGraph(5, 5),
        cost,
        heuristic,
        Pos(0, 0),
        Pos(4, 4)
      )

    assert(path.head == Pos(0, 0))
    assert(path.last == Pos(4, 4))

    // Manhattan distance = 8 moves
    // therefore 9 nodes in path
    assert(path.size == 9)

    // verify all steps are adjacent
    path.sliding(2).foreach {
      case List(a, b) =>
        val dist =
          math.abs(a.x - b.x) +
            math.abs(a.y - b.y)

        assert(dist == 1)

      case _ =>
    }
  }
}

