package com.myway.adventofcode.adv2025

import com.myway.adventofcode.tools.astar.{Astar, AstarGraph}
import com.myway.adventofcode.tools.file.FileUtil
import com.myway.adventofcode.tools.parser.ParserErrorMonad._

object Day10 {

  def main(args: Array[String]): Unit = {
    val data: List[String] = FileUtil.readFile("adventofcode/2025/day10.txt")
    println(solve1(data))
    println(solve2(data))
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
      println(s"j=${joltage.mkString(".")}")
      val g = new AstarGraph[List[Int]] {
        override def next(is: List[Int]): List[List[Int]] = buttons.map(_.pressJolt(is))
      }
      val cost: List[Int] => List[Int] => Long = x => y => 1L
      val out = Astar.find(g, cost, joltage.map(_ => 0), joltage)

      out.size - 1
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
  }

  val subParser: ParserError[Button] = for {
    _ <- char('(')
    ids <- many(tokenOf(',', digits))
    _ <- char(')')
  } yield Button(ids.map(_.toInt))

  def breadthFirstSearch[S](generator: S => (S, List[S]), p: S => Boolean): S => Option[S] =
    s => breadthFirstGenerator(generator)(s).find(p)

  def breadthFirstGenerator[S](generator: S => (S, List[S])): S => LazyList[S] =
    s => bfs0[S](generator, s)

  private def bfs0[S](g: S => (S, List[S]), s: S): LazyList[S] =
    LazyList.unfold[S, (List[S], Set[S])]((s :: Nil, Set()))(f = pair => pair._1 match {
      case Nil => None
      case y :: ys =>
        val (a, newS) = g(y)
        val set: Set[S] = pair._2 + a
        Some(a, (ys ++ newS.filterNot(set.contains), set))
    })

}
