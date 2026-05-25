package com.myway.codinggame.solo

object SandFall {

  case class Point(x: Int, y: Int)

  def solve(l: List[String]): List[String] = {
    val Array(w, h) = (l.head split " ").filter(_ != "").map(_.toInt)
    val n           = l.tail.head.toInt
    val list: List[(Char, Point)] = l.tail.tail
      .map(_.split(" "))
      .map(ar => (ar(0).charAt(0), Point(ar(1).toInt, h+1)))
    val map: Map[Point, Char] = Map()

    val finalMap: Map[Point, Char] = list.foldLeft(map) { (acc, pair) =>
      val point  = pair._2
      val chr    = pair._1
      val fallen = fall(w, acc, chr)(point)
      acc + (fallen -> chr)
    }

    List
      .range(0, h)
      .reverse
      .map(y =>
        s"|${List.range(0, w).map(x => finalMap.getOrElse(Point(x, y), ' ')).mkString}|"
      ) ++ List(
      s"+${List.range(0, w).map(_ => '-').mkString}+"
    )
  }

  def fall(w: Int, map: Map[Point, Char], chr: Char): Point => Point = p => fallRec(w, map, chr, p)

  private def fallRec(w: Int, map: Map[Point, Char], chr: Char, pt: Point): Point =
    if (pt.y.equals(0)) pt
    else {
      val next = pt.copy(y = pt.y - 1)
      if (map.contains(next)) {
        if (chr.isLower) {
          val next1 = pt.copy(pt.x + 1, y = pt.y - 1)
          if (map.contains(next1) || next1.x >= w) {
            val next2 = pt.copy(pt.x - 1, y = pt.y - 1)
            if(map.contains(next2) || next2.x<0) {
              pt
            } else  fallRec(w,map, chr, next2)

          } else fallRec(w,map, chr, next1)

        } else {
          val nexT2 = pt.copy(pt.x - 1, y = pt.y - 1)
          if(map.contains(nexT2) || nexT2.x<0) {
            val nexT1 = pt.copy(pt.x + 1, y = pt.y - 1)

            if (map.contains(nexT1) || nexT1.x >= w) {
              pt
            } else fallRec(w,map, chr, nexT1)

          } else  fallRec(w,map, chr, nexT2)


        }
      } else fallRec(w,map, chr, next)
    }
}
