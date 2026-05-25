package com.myway.codinggame.solo

object JetCoaster {

  def solve(l: Int, c: Int, n: Int, list: List[Int]): Long =
    solveRec(l, n, State(c, list, 0L), Map()).acc

  import scala.annotation.tailrec

  case class State(c: Int, list: List[Int], acc: Long)

  @tailrec
  private def solveRec(l: Int, n: Int, state: State, map: Map[List[Int], List[State]]): State =
    if (state.c <= 0) state
    else {
      val cumul: List[(Int, Int)] = state.list
        .foldLeft[List[(Int, Int)]](Nil) { (acc, x) =>
          val s = acc.headOption.map(_._2).getOrElse(0)
          (x, s + x) :: acc
        }
        .reverse
      val nextBatch: List[Int] = cumul.takeWhile(_._2 <= l).map(_._1)

      val newState =
        State(
          state.c - 1,
          state.list.drop(nextBatch.size) ++ nextBatch,
          state.acc + nextBatch.sum
        )

      val states = map.getOrElse(newState.list, Nil)
      val newMap = map - newState.list + (newState.list -> (newState :: states))

      newMap.keySet.find(k => newMap.getOrElse(k, Nil).size > 1) match {
        case Some(k) =>
          val twoStates = newMap(k).sortBy(_.acc)
          val s1        = twoStates.head
          val s2        = twoStates.tail.head
          val diffy     = s2.acc - s1.acc
          val diffx     = s2.c - s1.c
          val q         = s2.c / diffx
          val r         = s2.c % diffx
          val total     = s2.acc - diffy * q
          solveRec(l, n, State(r, k, total), Map())
        case None => solveRec(l, n, newState, newMap)
      }

    }

}
