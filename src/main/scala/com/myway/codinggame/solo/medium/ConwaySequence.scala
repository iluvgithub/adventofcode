package com.myway.codinggame.solo.medium

// https://www.codingame.com/ide/puzzle/conway-sequence
object ConwaySequence {

  def solve(init: Int, target: Int): String =
    List
      .range(0, target - 1)
      .foldLeft(List(init))((acc, _) => handleListRec(1, acc.head, acc.tail, Nil))
      .mkString(" ")

  def handleListRec(cumul: Int, current: Int, l: List[Int], out: List[Int]): List[Int] = l match {
    case Nil => out ++ List(cumul, current)
    case x :: xs =>
      if (current == x) handleListRec(cumul + 1, current, xs, out)
      else handleListRec(1, x, xs, out ++ List(cumul, current))
  }
}
