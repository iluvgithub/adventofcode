package com.myway.adventofcode.tools.point

trait Direction {
  def move(p: Point): Point
  def turnLeft: Direction
  def opposite: Direction
}

object North extends Direction {
  override def move(p: Point): Point = p.copy(y = p.y - 1)

  override def turnLeft: Direction = West

  override def opposite: Direction = South
}

object South extends Direction {
  override def move(p: Point): Point = p.copy(y = p.y + 1)

  override def turnLeft: Direction = East

  override def opposite: Direction = North
}

object East extends Direction {
  override def move(p: Point): Point = p.copy(x = p.x + 1)

  override def turnLeft: Direction = North

  override def opposite: Direction = West
}

object West extends Direction {
  override def move(p: Point): Point = p.copy(x = p.x - 1)

  override def turnLeft: Direction = South

  override def opposite: Direction = East
}

object Direction {

  val allFour: List[Direction]  = List(North, East, South, West)
  val diagonal: List[Direction] = allFour.map(_.turnLeft)
}
