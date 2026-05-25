package com.myway.codinggame.solo

import scala.annotation.tailrec

object RectangularBlockSpinner {

  def solve(in: List[String]): List[String] = {
    val angle = in.tail.head.toInt
    solve(in.tail.tail.map(_.filterNot(_.equals(' '))), angle)
  }

  @tailrec
  def solve(l: List[String], angle: Int): List[String] = if (angle < 0) solve(l, angle + 360)
  else {
    if (angle > 90)
      solve(transpose(l), angle - 90)
    else
      solve2(l)
  }

  def transpose(l: List[String]): List[String] =
    List
      .range(0, l.size)
      .reverse
      .map(j =>
        List
          .range(0, l.size)
          .map(i => l(i).charAt(j))
          .mkString
      )

  def solve2(in: List[String]): List[String] = {
    val originSize = in.length
    val sz         = in.head.length * 2 - 1

    for {
      j <- List.range(0, sz).reverse
    } yield List
      .range(0, sz)
      .map { i =>
        if ((i + j + originSize) % 2 == 0) ' '
        else {
          val x = (i - j + originSize - 1) / 2
          val y = (j + i - originSize + 1) / 2
          if (x >= 0 && y >= 0 && x < originSize && y < originSize) in(x).charAt(y) else ' '
        }
      }
      .mkString

  }

}
