package com.myway.codinggame.solo.medium

object RectMax {

  case class Point(x: Int, y: Int)

  def solve(l: List[String]): Int = {
    val Array(w, h) = (l.head split " ").filter(_ != "").map(_.toInt)

    val pointsScores: Map[Point, Int] = List
      .range(0, h)
      .flatMap { y =>
        val arr = l.tail.drop(y).head.split(" ").toList.map(_.toInt)
        List.range(0, w).map(x => Point(x, y) -> arr(x))
      }
      .foldLeft[Map[Point, Int]](Map())((acc, t3) => acc + (t3._1 -> t3._2))

    solve(pointsScores, w, h)
  }

  def solve(pointsScores: Map[Point, Int], w: Int, h: Int): Int = {

    val xij: List[(Int, Int)] = for {
      xi <- List.range(0, w)
      xj <- List.range(0, w)
      if xi <= xj
    } yield (xi, xj)

    val horizontals: List[Map[Point, Int]] = List
      .range(0, h)
      .map { y =>
        val out: Map[Point, Int] = xij.map { case (xi, xj) =>
          (Point(xi, xj), List.range(xi, xj + 1).map(x => pointsScores(Point(x, y))).sum)
        }.toMap
        out
      }

    val o: List[List[Int]] = xij
      .map { case (xi, xj) => List.range(0, h).map(y => horizontals(y)(Point(xi, xj))) }

    o.map(mss).max

  }

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
  def mss(l: List[Int]): Int = {
    val o = maxSegSum(l)
    if (o.isEmpty) l.max else o.sum
  }

}
