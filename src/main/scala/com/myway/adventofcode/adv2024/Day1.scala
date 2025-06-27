package com.myway.adventofcode.adv2024

import com.myway.adventofcode.tools.bag.Bag
import com.myway.adventofcode.tools.file.FileUtil

object Day1 {

  def main(args: Array[String]): Unit = {
    val data: List[String] = FileUtil.readFile("adventofcode/2024/day1.txt")
    println(solve1(data))
    println(solve2(data))
  }

  def solve1(data: List[String]): String = {
    val ints = data.map(toInts)
    val l = ints.map(_._1).sorted
    val r = ints.map(_._2).sorted
    val z = l.zip(r).map({ case (u, v) => Math.abs(v - u) }).sum
    z.toString
  }

  private def toInts(s: String): (Int, Int) = {
    val ls = s.split("\\s+").toList.map(_.trim.toInt)
    (ls.head, ls.tail.head)
  }

  def solve2(data: List[String]): String = {
    val ints = data.map(toInts)
    val l: List[Int] = ints.map(_._1).sorted
    val r: List[Int] = ints.map(_._2).sorted
    val bag: Bag[Int] = r.foldLeft(new Bag[Int]())(_ add _)
    val out = l.map(i => i * bag.get(i)).sum
    out.toString
  }

}
