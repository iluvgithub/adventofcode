package com.myway.codinggame.solo.easy

object ButtonMash {

  def transform(n: Int): Set[Int] =
    (n + 1 :: n - 1 :: n * 2 :: Nil).toSet.filter(_ >= 0)

  case class Step(id: Int, ls: Set[Int], found: Boolean) {
    def next(tgt: Int): Step = {

      val newSet: Set[Int] = ls.flatMap(transform).
        filterNot(ls).
        filterNot( _ < tgt* 3)

      Step(id + 1, newSet, newSet.contains(tgt))
    }

  }

  def solve(target: Int): Int =
    if (target == 0) 0
    else
      LazyList
        .iterate(Step(0, Set(0), found = false))(_.next(target))
        .find(_.found)
        .get
        .id

}
