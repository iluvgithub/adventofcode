package com.myway.codinggame.solo.easy

object ButtonMash {

  def transform(n: Int): List[Int] =
    (n + 1 :: n - 1 :: n * 2 :: Nil).filter(_ >= 0)

  case class Step(n: Int, ls: List[Int]) {
    def next: Step = Step(
      n + 1,
      ls.flatMap(transform)
    )

  }

  def solve(in: Int): Int =
    LazyList.iterate(Step(0, 0 :: Nil))(_.next).find(_.ls.contains(in)).get.n

}
