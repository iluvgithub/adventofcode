package com.myway.adventofcode.tools.point

trait Direction {
  def move(p: Point): Point
}

object North extends Direction {
  override def move(p: Point): Point = p.copy(y = p.y - 1)
}

object South extends Direction {
  override def move(p: Point): Point =  p.copy(y = p.y + 1)
}

object East extends Direction {
  override def move(p: Point): Point =  p.copy(x = p.x + 1)
}

object West extends Direction {
  override def move(p: Point): Point = p.copy(x = p.x - 1)
}