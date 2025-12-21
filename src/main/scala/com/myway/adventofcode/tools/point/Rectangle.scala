package com.myway.adventofcode.tools.point

case class Rectangle(minX: Int, maxX: Int,
                     minY: Int, maxY: Int) {


  def isInside(p: Point): Boolean =
    minX <= p.x && p.x <= maxX &&
      minY <= p.y && p.y <= maxY

  def isInterior(p: Point): Boolean =
    minX < p.x && p.x < maxX &&
      minY < p.y && p.y < maxY
}

object Rectangle {
  def from(p: Point, q: Point): Rectangle = Rectangle(
    Math.min(p.x, q.x), Math.max(p.x, q.x),
    Math.min(p.y, q.y), Math.max(p.y, q.y)
  )
}