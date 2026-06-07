package com.myway.codinggame.solo.easy

object SixDegreesKevinBacon {

  import scala.collection.mutable

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

  val KEVIN = "Kevin Bacon"

  def solve(in: List[String]): Int = {
    val target = in.head
    val l      = in.tail.tail
    val movToAct: Map[String, List[String]] = l.foldLeft(Map[String, List[String]]()) { (map, s) =>
      val array  = s.split(":")
      val key    = array(0)
      val actors = array(1).split(", ").toList.map(_.trim)
      map + (key -> actors)
    }
    val actorToMovies: Map[String, List[String]] =
      movToAct.keySet.toList.foldLeft(Map[String, List[String]]())((actToMov, movie) =>
        movToAct(movie).foldLeft(actToMov) { (map, act) =>
          val list = movie :: map.getOrElse(act, Nil)
          map - act + (act -> list.distinct)
        }
      )

    val astarGraph = new AstarGraph[String] {

      override def next(actor: String): List[String] = for {
        movie      <- actorToMovies.getOrElse(actor, Nil)
        otherActor <- movToAct.getOrElse(movie, Nil)
      } yield otherActor
    }
    val cost: String => String => Long = _ => _ => 1L
    val out: List[String]              = Astar.find(astarGraph, cost, KEVIN, target)
    out.size - 1
  }
}
