package com.myway.adventofcode.tools.point

trait Bipoint {
  val mid: Point
}

object Bipoint {
  def from(p1: Point, p2: Point): Option[Bipoint] = if (p1.x.equals(p2.x))
    Some(VertBipoint(p1.x, Math.min(p1.y, p2.y), Math.max(p1.y, p2.y)))
  else if (p1.y.equals(p2.y))
    Some(HorBipoint(Math.min(p1.x, p2.x), Math.max(p1.x, p2.x), p1.y))
  else None
}

case class HorBipoint(fromX: Int, toX: Int, y: Int) extends Bipoint {

  override val mid: Point = Point((fromX + toX) / 2, y)
}

case class VertBipoint(x: Int, fromY: Int, toY: Int) extends Bipoint {

  override val mid: Point = Point(x, (fromY + toY) / 2)
}
