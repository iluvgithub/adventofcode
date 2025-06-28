package com.myway.adventofcode.adv2024

import com.myway.adventofcode.tools.file.FileUtil
import com.myway.adventofcode.tools.parser.ParserErrorMonad.{ParserErrorWrapper, many, number, token}

object Day2 {

  def main(args: Array[String]): Unit = {
    val data: List[String] = FileUtil.readFile("adventofcode/2024/day2.txt")
    println(solve1(data))
    println(solve2(data))
  }

  def solve1(data: List[String]): Int = data.map(parse).count(isSafe)


  def parse(s: String): List[Long] = many(token(number)).parse(s).fold(_ => Nil, identity)

  private def isSafe(is: List[Long]): Boolean = {
    val q: List[Long] = is.zip(is.tail).map({ case (u, v) =>  u - v  })
    val b1 = q.map(Math.abs).count(x => x > 0 && x <= 3).equals(q.size)
    val b2 = q.forall(_ >= 0) || q.forall(_ <= 0)
    b1 && b2
  }

  def solve2(data: List[String]): Int = data.map(parse).count(isSafe2)

  private def isSafe2(is: List[Long]): Boolean = {
    val iss: List[List[Long]] = List.range(0,is.size).map(
      i => is.zipWithIndex.filterNot(_._2.equals(i)).map(_._1)
    )
    iss.map(isSafe).foldLeft(false)(_ ||_)
  }
}
