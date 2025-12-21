package com.myway.adventofcode.tools.point

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RectangleTest extends AnyFunSuite with Matchers {

  test(" inside") {
    val r = Rectangle.from(Point(1, 2), Point(3, 4))
    r.isInside(Point(3, 3)) shouldBe true
    r.isInside(Point(1, 2)) shouldBe true
  }

  test(" interior") {
    val r = Rectangle.from(Point(1, 2), Point(3, 4))
    r.isInterior(Point(3, 3)) shouldBe true
    r.isInterior(Point(1, 2)) shouldBe false
  }
}
