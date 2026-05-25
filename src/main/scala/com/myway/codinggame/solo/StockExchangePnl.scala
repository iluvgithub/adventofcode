package com.myway.codinggame.solo

object StockExchangePnl {

  def maxSegSum(l: List[Int]): List[Int] = {
    val o: List[(Int, List[Int])] = l.scanLeft[(Int, List[Int])]((0, Nil)) { (acc, i) =>
      val sum: Int = acc._1 + i
      if (sum < 0) (0, Nil)
      else {
        (sum, i :: acc._2)
      }
    }
    o.maxBy(_._1)._2.reverse
  }

  def solve(l: List[Int]): Int = {

    val diff: List[Int] = l.zip(l.tail).map { case (u, v) => v - u }

    -maxSegSum(diff.map(_ * -1)).sum
  }
}
