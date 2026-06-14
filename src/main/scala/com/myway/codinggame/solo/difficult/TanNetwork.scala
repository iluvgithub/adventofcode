package com.myway.codinggame.solo.difficult

// https://www.codingame.com/ide/puzzle/tan-network
object TanNetwork {

  trait Graph[A] {
    def next(node: A): List[A]
  }
  object Dijkstra {

    def find[A](
      g: Graph[A],
      cost: A => A => Double,
      from: A,
      to: A
    ): List[A] = {

      @annotation.tailrec
      def loop(
        frontier: Set[A],
        distances: Map[A, Double],
        previous: Map[A, A]
      ): List[A] =
        if (frontier.isEmpty) {
          Nil
        } else {
          val current =
            frontier.minBy(node => distances.getOrElse(node, Double.PositiveInfinity))

          if (current == to) {
            reconstructPath(previous, to)
          } else {
            val currentDistance = distances(current)

            val (newDistances, newPrevious, newFrontier) =
              g.next(current)
                .foldLeft(
                  (distances, previous, frontier - current)
                ) { case ((dist, prev, front), neighbor) =>
                  val alt = currentDistance + cost(current)(neighbor)

                  if (alt < dist.getOrElse(neighbor, Double.PositiveInfinity)) {
                    (
                      dist.updated(neighbor, alt),
                      prev.updated(neighbor, current),
                      front + neighbor
                    )
                  } else {
                    (dist, prev, front)
                  }
                }

            loop(newFrontier, newDistances, newPrevious)
          }
        }

      def reconstructPath(previous: Map[A, A], target: A): List[A] = {
        @annotation.tailrec
        def build(current: A, acc: List[A]): List[A] =
          previous.get(current) match {
            case Some(parent) => build(parent, current :: acc)
            case None         => current :: acc
          }

        build(target, Nil)
      }

      if (from == to) List(from)
      else
        loop(
          frontier = Set(from),
          distances = Map(from -> 0.0),
          previous = Map.empty
        )
    }
  }

  def parse(s: String): Node = {
    val xs = s.split(",")
    Node(
      xs(0),
      xs(1).toList.filterNot(_.equals('"')).mkString,
      xs(3).toDouble,
      xs(4).toDouble,
      xs(7).toInt
    )
  }
  case class Node(id: String, name: String, lat: Double, long: Double, stopType: Int) {
    def dist(that: Node): Double = {
      val x = (that.long - this.long) * Math.cos((toRadian(this.lat) + toRadian(that.lat)) / 2)
      val y = that.lat - this.lat
      6371.0 * Math.sqrt(x * x + y * y)
    }
    private def toRadian(degr: Double): Double = 2 * Math.PI * degr / 360.0
  }
  case class Edge(from: String, to: String) {
    def swap = Edge(to, from)
  }
  def parseEdge(s: String): Edge = {
    val arr = s.split(" ")
    Edge(arr(0), arr(1))
  }
  def solve(l: List[String]): List[String] = {
    val to                              = l.head
    val from                            = l.tail.head
    val n                               = l.drop(2).head.toInt
    val nodes: List[Node]               = l.drop(3).take(n).map(parse)
    val mapNode                         = nodes.map(n => (n.id, n)).toMap
    val m                               = l.drop(3 + n).head.toInt
    val edges: List[Edge]               = l.drop(3 + n + 1).take(m).map(parseEdge).map(_.swap)
    val liaise: Map[String, List[Edge]] = edges.groupBy(_.from)

    val gr = new Graph[Node] {
      override def next(node: Node): List[Node] =
        liaise
          .getOrElse(node.id, Nil)
          .map(_.to)
          .map(i => mapNode.get(i))
          .filter(_.isDefined)
          .map(_.get)
    }

    val cost: Node => Node => Double = n1 => n2 => n1.dist(n2)
    val targetNode                   = mapNode(to)

    val out: List[Node] = Dijkstra.find(gr, cost, mapNode(from), targetNode)
    if (out.isEmpty) List("IMPOSSIBLE")
    else out.map(_.name).reverse
  }
}
