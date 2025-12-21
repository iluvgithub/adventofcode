package com.myway.adventofcode.adv2025

import com.myway.adventofcode.tools.astar.{Astar, AstarGraph}
import com.myway.adventofcode.tools.file.FileUtil
import com.myway.adventofcode.tools.linear.UndeterminedSolver
import com.myway.adventofcode.tools.parser.ParserErrorMonad._

object Day10 {

  def main(args: Array[String]): Unit = {
    val data: List[String] = FileUtil.readFile("adventofcode/2025/day10.txt")
    println(solve1(data))
    println(solve2(data)) // 16855 too low
  }

  def solve1(data: List[String]): Long = {
    val machines: List[Machine] = data.map(machineParser.parseSure)
    machines.map(_.solve).sum
  }

  def solve2(data: List[String]): Long = {
    val machines: List[Machine] = data.map(machineParser.parseSure)
    machines.map(_.solve2).sum
  }


  case class Machine(light: List[Boolean], buttons: List[Button], joltage: List[Int]) {

    def solve: Long = {

      val g = new AstarGraph[List[Boolean]] {
        override def next(bs: List[Boolean]): List[List[Boolean]] = buttons.map(_.press(bs))
      }
      val cost: List[Boolean] => List[Boolean] => Long = x => y => 1L
      val out = Astar.find(g, cost, light.map(_ => false), light)
      out.size - 1
    }
    def solve2: Long = {
      val sz = buttons.flatMap(_.ids).max + 1
      val A0: Array[Array[Double]] = buttons.toArray.map(_.toArray(sz))
      val n = A0.map(_.length).max
      val m = A0.length
      val A = List.range(0, n).map(j =>
        List.range(0, m).map(i => A0(i)(j)).toArray
      ).toArray

      val b1: Array[Double] = joltage.map(_.toDouble).toArray

      val solution1: Array[Double] = UndeterminedSolver.solve(A, b1)
      solution1.map(Math.round).sum

    }

  }


  def machineParser: ParserError[Machine] = for {
    _ <- char('[')
    light <- many(orElse(char('.').map(_ => false), char('#').map(_ => true)))
    _ <- char(']')
    _ <- char(' ')
    buttons <- many(token(subParser))
    _ <- token(char('{'))
    joltage <- token(many(tokenOf(',', digits)))
    _ <- char('}')

  } yield Machine(light, buttons, joltage.map(_.toInt))

  case class Button(ids: List[Int]) {
    def press(bs: List[Boolean]): List[Boolean] = bs.zipWithIndex.map(
      x => if (ids.toSet.contains(x._2)) !x._1 else x._1
    )

    def pressJolt(is: List[Int]): List[Int] = is.zipWithIndex.map(
      x => if (ids.toSet.contains(x._2)) 1 + x._1 else x._1
    )

    def toArray(sz: Int): Array[Double] = List.range(0, sz).map(
      i => if (ids.contains(i)) 1.0 else 0.0
    ).toArray
  }

  val subParser: ParserError[Button] = for {
    _ <- char('(')
    ids <- many(tokenOf(',', digits))
    _ <- char(')')
  } yield Button(ids.map(_.toInt))


}
