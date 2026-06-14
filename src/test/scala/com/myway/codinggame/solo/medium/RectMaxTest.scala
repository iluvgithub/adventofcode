package com.myway.codinggame.solo.medium

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/max-rect
class RectMaxTest extends AnyFunSuite with Matchers {

  import RectMax._

  test(" ") {
    val in = List(
      "2 2",
      "5 -9",
      "6 9")
    solve(in) shouldBe 15
  }
}
