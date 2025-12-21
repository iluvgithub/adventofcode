package com.myway.adventofcode.adv2025

import com.myway.adventofcode.tools.file.FileUtil
import com.myway.adventofcode.tools.point.Point

object Day9 {

  def main(args: Array[String]): Unit = {
    val data: List[String] = FileUtil.readFile( "adventofcode/2025/day9.txt")
    println(solve1(data))
    println(solve2(data))
  }
  def solve1(data: List[String]): Long = {
    val pts: List[Point] = Point.fromList(data)

    solve(pts, _ => true)
  }

  private def solve(pts: List[Point], pred: ((Point, Point)) => Boolean): Long = {
    val pairs: List[(Point, Point)] = for {
      i <- List.range(0, pts.size)
      j <- List.range(0, pts.size)
      if i < j
    } yield (pts(i), pts(j))

    pairs.filter(pred).map({ case (u, v) => surf(u, v) }).max
  }

  private def surf(p: Point, q: Point): Long = {
    val minX = Math.min(p.x, q.x)
    val maxX = Math.max(p.x, q.x)

    val minY = Math.min(p.y, q.y)
    val maxY = Math.max(p.y, q.y)
    (maxX - minX + 1L) * (maxY - minY + 1L)
  }


  def solve2(data: List[String]): Long = {
    val pts: List[Point] = Point.fromList(data)
    0L
  }

}
