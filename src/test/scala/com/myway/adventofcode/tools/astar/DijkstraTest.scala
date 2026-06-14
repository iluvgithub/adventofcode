package com.myway.adventofcode.tools.astar

import com.myway.codinggame.solo.difficult.TanNetwork
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DijkstraTest  extends AnyFunSuite with Matchers {

  import Dijkstra._

  test("find shortest path") {

    val graph = new Graph[String] {
      private val edges = Map(
        "A" -> List("B", "C"),
        "B" -> List("D"),
        "C" -> List("D"),
        "D" -> List("E"),
        "E" -> Nil
      )

      override def next(node: String): List[String] =
        edges.getOrElse(node, Nil)
    }

    val weights: Map[(String, String), Double] = Map(
      ("A", "B") -> 1.0,
      ("A", "C") -> 4.0,
      ("B", "D") -> 2.0,
      ("C", "D") -> 1.0,
      ("D", "E") -> 1.0
    )

    val cost: String => String => Double =
      from => to => weights((from, to))

    val path =
      Dijkstra.find(graph, cost, "A", "E")

    assert(path == List("A", "B", "D", "E"))
  }

  test("returns empty list when destination unreachable") {

    val graph = new Graph[Int] {
      override def next(node: Int): List[Int] =
        node match {
          case 1 => List(2)
          case 2 => Nil
          case 3 => Nil
        }
    }

    val cost: Int => Int => Double =
      _ => _ => 1.0

    val path =
      Dijkstra.find(graph, cost, 1, 3)

    assert(path.isEmpty)
  }

  test("returns singleton path when source equals destination") {

    val graph = new Graph[Int] {
      override def next(node: Int): List[Int] = Nil
    }

    val cost: Int => Int => Double =
      _ => _ => 1.0

    assert(
      Dijkstra.find(graph, cost, 42, 42) == List(42)
    )
  }
}
