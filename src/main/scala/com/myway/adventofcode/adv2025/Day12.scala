package com.myway.adventofcode.adv2025

import com.myway.adventofcode.tools.file.FileUtil
import com.myway.adventofcode.tools.parser.ParserErrorMonad._
import com.myway.adventofcode.tools.point.Point

object Day12 {

  def main(args: Array[String]): Unit = {
    val data: List[String] = FileUtil.readFile("adventofcode/2025/day12.txt")
    println(solve1(data))
    println(solve2(data))
  }

  def solve1(data: List[String]): Long = {
    val problem = parse(data)

    val shapeMaps: Map[Int, Shape] = problem.shapes.map(sh => sh.id -> sh).toMap

    println(" ")
    problem.grids.foreach(gr =>
      println(s"surf = ${gr.surf} nb shapes = ${gr.ids.sum} place:${gr.ids.zipWithIndex.map {
          case (nb, id) => nb * shapeMaps(id).nbSharp
        }.sum}")
    )

    0L
  }

  def solve2(data: List[String]): Long = 0L

  def parse(data: List[String]): Problem = parse(data, Problem(Nil, Nil))

  def parse(data: List[String], pb: Problem): Problem = data match {
    case Nil => pb.copy(shapes = pb.shapes.reverse, grids = pb.grids.reverse)
    case x :: xs =>
      parse(
        xs,
        if (x.isEmpty) pb
        else
          idParse.parseOpt(x) match {
            case Some(shape) => pb.copy(shapes = shape :: pb.shapes)
            case None =>
              gridParser.parseOpt(x) match {
                case Some(grid) => pb.copy(grids = grid :: pb.grids)
                case None =>
                  val shape1: Shape = pb.shapes.head
                  val y    = if (shape1.map.isEmpty) 0 else shape1.map.keySet.map(_.y).max + 1
                  val tail = pb.shapes.tail
                  val newMap =
                    x.toCharArray.toList.zipWithIndex.foldLeft[Map[Point, Char]](shape1.map)(
                      (acc, t2) => acc + (Point(t2._2, y) -> t2._1)
                    )
                  pb.copy(shapes = shape1.copy(map = newMap) :: tail)
              }
          }
      )
  }

  case class Problem(shapes: List[Shape], grids: List[Grid])

  case class Shape(id: Int, map: Map[Point, Char]) {
    def nbSharp: Int = map.values.count(_.equals('#'))
  }

  case class Grid(m: Int, n: Int, ids: List[Int]) {
    def surf: Int = m * n
  }

  val idParse: ParserError[Shape] = for {
    id <- digits
    _  <- char(':')
  } yield Shape(id.toInt, Map())

  val gridParser: ParserError[Grid] = for {
    m <- digits
    _ <- char('x')
    n <- digits
    _ <- char(':')
    l <- many(token(digits))
  } yield Grid(m.toInt, n.toInt, l.map(_.toInt))

  def parseGrid(s: String): Grid = gridParser.parseSure(s)

}
