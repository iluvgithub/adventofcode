package com.myway.codinggame.solo

object JumpTheQueue {

  case class Group(ids: List[Int]) {
    private val set: Set[Int]     = ids.toSet
    def contains(i: Int): Boolean = set.contains(i)
  }

  def solve(l: List[String]): List[String] = {
    val Array(g, e) = (l.head split " ").filter(_ != "").map(_.toInt)

    val groups = l.tail.dropRight(1).foldLeft[List[Group]](Nil) { (acc, s) =>
      acc ++ List(Group(s.split(" ").map(_.toInt).toList))
    }
    val groupMap: Map[Int, Group] = groups.foldLeft[Map[Int, Group]](Map())((m, grp) =>
      grp.ids.foldLeft(m)((mm, i) => mm ++ Map(i -> grp))
    )

    val inputs: List[Int] = l.last.split(" ").map(_.toInt).toList
    val oo = inputs.foldLeft[(List[Int], List[Int])]((Nil, Nil))((acc, i) =>
      handleQueue(groupMap)(acc._1)(acc._2)(i)
    )

    oo._2.map(_.toString)
  }

  def handleQueue(
    groupMap: Map[Int, Group]
  ): List[Int] => List[Int] => Int => (List[Int], List[Int]) = q =>
    out =>
      id0 =>
        if (id0 == -1) (q.tail, out ++ List(q.head))
        else {
          val o: Option[Int] =
            List.range(0, q.size).findLast(id => groupMap.get(q(id)).exists(_.contains(id0)))
          o match {
            case None      => (q ++ List(id0), out)
            case Some(id1) => (q.take(id1 + 1) ++ (id0 :: q.drop(id1 + 1)), out)
          }
        }

}
