package com.myway.codinggame.solo.easy

object DwarfStanding {

  def solve(l: List[String]): Int =
    solveEdges(l.tail.map { s =>
      val arr = s.split(" ").map(_.toInt)
      (arr(0), arr(1))
    })

  def solveEdges(l: List[(Int, Int)]): Int = {
    val nodes = l.flatMap(x => x._1 :: x._2 :: Nil).distinct
    val mp: Map[Int, (List[Int], Int)] =
      l.groupBy(_._1).map { case (u, us) => (u, (us.map(_._2), 0)) }
    val mp0 = nodes.filterNot(mp.keySet.contains).foldLeft(mp)((acc, kk) => acc + (kk -> (Nil, 1)))
    go(mp0).values.max
  }

  def hasSolution(mp0: Map[Int, (List[Int], Int)]): Int => Boolean = i => mp0(i)._1.isEmpty

  def go(mp0: Map[Int, (List[Int], Int)]): Map[Int, Int] =
    mp0.keySet.toList.filterNot(hasSolution(mp0)) match {
      case Nil => mp0.toList.map { case (k, v) => (k, v._2) }.toMap
      case x :: xs =>
        val keys         = x :: xs
        val candidateKey = keys.find(k => mp0(k)._1.forall(hasSolution(mp0))).get
        val v            = mp0(candidateKey)._1
        val vMax         = v.map(k => mp0(k)._2).max
        val mp1          = mp0 - candidateKey + (candidateKey -> (Nil, vMax+1))

        go(mp1)
    }
}
