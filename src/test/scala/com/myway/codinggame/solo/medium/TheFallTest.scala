package com.myway.codinggame.solo.medium

import com.myway.codinggame.solo.difficult.variant.TheResistance
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/the-fall-episode-1
class TheFallTest  extends AnyFunSuite with Matchers {

  import TheFall._

  test(" go") {
    val in = List(
      "4 3",
      "12 10",
      "11 5",
      "2 3")

    val list: List[List[Int]] = init(in)
    solve("TOP", list)(Point(1,0)).display shouldBe "1 1"
  }


}
