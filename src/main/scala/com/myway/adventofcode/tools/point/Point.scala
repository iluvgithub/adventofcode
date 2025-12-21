package com.myway.adventofcode.tools.point

case class Point(x: Int, y: Int) {

  def allFour: List[Point] = Direction.allFour.map(_.move(this))

}
