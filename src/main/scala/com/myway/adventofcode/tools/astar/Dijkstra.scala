package com.myway.adventofcode.tools.astar
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
            ): List[A] = {

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
            g.next(current).foldLeft(
              (distances, previous, frontier - current)
            ) {
              case ((dist, prev, front), neighbor) =>
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
    else loop(
      frontier = Set(from),
      distances = Map(from -> 0.0),
      previous = Map.empty
    )
  }
}
