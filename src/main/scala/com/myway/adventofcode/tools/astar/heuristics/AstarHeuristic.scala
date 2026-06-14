package com.myway.adventofcode.tools.astar.heuristics

trait AstarGraph[A] {
  def next(node: A): List[A]
}

object AstarHeuristic {
  import scala.collection.mutable
  def find[A](
               g: AstarGraph[A],
               cost: A => A => Double,
               heuristic: A => A => Double,
               from: A,
               to: A
             ): List[A] = {

    val gScore   = mutable.Map[A, Double]().withDefaultValue(Double.MaxValue)
    val fScore   = mutable.Map[A, Double]().withDefaultValue(Double.MaxValue)
    val cameFrom = mutable.Map[A, A]()

    gScore(from) = 0.0
    fScore(from) = heuristic(from)(to)

    implicit val ordering: Ordering[(Double, A)] =
      Ordering.by[(Double, A), Double](-_._1)

    val openSet =
      mutable.PriorityQueue[(Double, A)]((fScore(from), from))

    val closed = mutable.Set[A]()

    while (openSet.nonEmpty) {

      val (_, current) = openSet.dequeue()

      if (current == to) {

        def reconstruct(node: A): List[A] =
          cameFrom.get(node) match {
            case None       => List(node)
            case Some(prev) => reconstruct(prev) :+ node
          }

        return reconstruct(current)
      }

      if (!closed.contains(current)) {

        closed += current

        for (neighbor <- g.next(current)) {

          val tentativeG =
            gScore(current) + cost(current)(neighbor)

          if (tentativeG < gScore(neighbor)) {

            cameFrom(neighbor) = current
            gScore(neighbor) = tentativeG

            val h = heuristic(neighbor)(to)

            fScore(neighbor) = tentativeG + h

            openSet += ((fScore(neighbor), neighbor))
          }
        }
      }
    }

    Nil
  }
}