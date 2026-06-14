package com.myway.codinggame.solo.medium

object RectMax {

  case class Point(x: Int, y: Int) {
    def left: Point  = this.copy(x = this.x - 1)
    def right: Point = this.copy(x = this.x + 1)
    def up: Point    = this.copy(y = this.y - 1)
    def down: Point  = this.copy(y = this.y + 1)
  }
  case class Rect(upLeft: Point, downRight: Point) {
    def left: (Rect, List[Point], Rect) = {
      val p = this.upLeft.left
      (this.copy(upLeft = p), List.range(upLeft.y, downRight.y + 1).map(y => Point(p.x, y)), this)
    }
    def right: (Rect, List[Point], Rect) = {
      val p = this.downRight.right
      (
        this.copy(downRight = p),
        List.range(upLeft.y, downRight.y + 1).map(y => Point(p.x, y)),
        this
      )
    }

    def up: (Rect, List[Point], Rect) = {
      val p = this.upLeft.up
      (this.copy(upLeft = p), List.range(upLeft.x, downRight.x + 1).map(x => Point(x, p.y)), this)
    }
    def down: (Rect, List[Point], Rect) = {
      val p = this.downRight.down
      (
        this.copy(downRight = p),
        List.range(upLeft.x, downRight.x + 1).map(x => Point(x, p.y)),
        this
      )
    }

    def isValid(w: Int, h: Int): Boolean =
      upLeft.y >= 0 && upLeft.x >= 0 && downRight.x < w && downRight.y < h

  }

  def solve(l: List[String]): Int = {
    val Array(w, h) = (l.head split " ").filter(_ != "").map(_.toInt)

    val target = Rect(Point(0, 0), Point(w - 1, h - 1))

    val pointsScores: Map[Point, Int] = List
      .range(0, h)
      .flatMap { y =>
        val arr = l.tail.drop(y).head.split(" ").toList.map(_.toInt)
        List.range(0, w).map(x => Point(x, y) -> arr(x))
      }
      .foldLeft[Map[Point, Int]](Map())((acc, t3) => acc + (t3._1 -> t3._2))

    val scores: Map[Rect, Int] = (
      for {
        x <- List.range(0, w)
        y <- List.range(0, h)
      } yield Rect(Point(x, y), Point(x, y)) -> pointsScores(Point(x, y))
    ).foldLeft[Map[Rect, Int]](Map())((acc, t2) => acc + (t2._1 -> t2._2))
    go(target, w, h, pointsScores, scores.keySet, scores)
  }

  def go(
    target: Rect,
    w: Int,
    h: Int,
    pointsScores: Map[Point, Int],
    front: Set[Rect],
    scores: Map[Rect, Int]
  ): Int = if (scores.contains(target)) scores.values.max
  else {
    val newRectangles: List[(Rect, List[Point], Rect)] =
      front
        .flatMap(r => List(r.up, r.down, r.left, r.right))
        .toList
        .filter(_._1.isValid(w, h))
        .filterNot(x=>scores.keySet.contains(x._1))
        .distinct

    val newFront = newRectangles.map(_._1).toSet

    val newScores = newRectangles.foldLeft(scores) { (acc, t3) =>
      val v: Int = t3._2.map(p => pointsScores(p)).sum
      val newV   = acc(t3._3) + v
      acc - t3._1 + (t3._1 -> newV)
    }
    go(target, w, h, pointsScores, newFront, newScores)
  }

}
