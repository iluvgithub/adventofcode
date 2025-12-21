package com.myway.adventofcode.tools.point

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BipointTest extends AnyFunSuite with Matchers {

  test(" vertical") {
    Bipoint.from(Point(10, 1), Point(10, 4)) shouldBe Some(VertBipoint(10, 1, 4))
    Bipoint.from(Point(10, 4), Point(10, 1)) shouldBe Some(VertBipoint(10, 1, 4))
    Bipoint.from(Point(10, 4), Point(10, 1)).get.mid shouldBe Point(10, 2)
    Bipoint.from(Point(10, 5), Point(10, 1)).get.mid shouldBe Point(10, 3)
  }

  test(" horizontal") {
    Bipoint.from(Point(1, 6), Point(10, 6)) shouldBe Some(HorBipoint(1, 10, 6))
    Bipoint.from(Point(10, 6), Point(1, 6)) shouldBe Some(HorBipoint(1, 10, 6))
  }

  test(" none") {
    Bipoint.from(Point(1, 1), Point(2, 2)) shouldBe None
  }

}
