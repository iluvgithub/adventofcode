package com.myway.codinggame.solo.medium

object TheGift {

  def solve(l: List[Int]): Either[String, List[Int]] = {
    val nb: Int            = l.head
    val giftValue: Int     = l.tail.head
    val budgets: List[Int] = l.drop(2)

    goGreedy(giftValue, budgets.zipWithIndex.map { case (v, k) => k -> v }.toMap, Map())
  }

  def goGreedy(
    giftValue: Int,
    budgets: Map[Int, Int],
    out: Map[Int, Int]
  ): Either[String, List[Int]] =
    if (giftValue <= 0) Right(out.values.toList.sorted)
    else if (budgets.isEmpty) Left("IMPOSSIBLE")
    else {
      val n        = budgets.size
      val min: Int = budgets.values.min
      val diff     = 1L * n * min

      if (diff <= 1L * giftValue) {
        val newGiftValue = giftValue - diff
        val (newBudgets, newOut) = budgets.toList.foldLeft((budgets, out))((acc, pair) =>
          (
            acc._1 - pair._1 + (pair._1 -> (budgets(pair._1) - min)),
            acc._2 - pair._1 + (pair._1 -> (out.getOrElse(pair._1, 0) + min))
          )
        )
        goGreedy(newGiftValue.toInt, newBudgets.toList.filter(_._2 > 0).toMap, newOut)
      } else {
        val q = giftValue / n
        val r = giftValue % n

        val o = budgets.keySet.toList.sorted.zipWithIndex.foldLeft(out) { (acc, ki) =>
          val v = q + (if (ki._2 < r) 1 else 0) + out.getOrElse(ki._1, 0)
          acc - ki._1 + (ki._1 -> v)
        }
        Right(o.values.toList.sorted)
      }
    }

}
