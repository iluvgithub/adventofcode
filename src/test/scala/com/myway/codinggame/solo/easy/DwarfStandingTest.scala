package com.myway.codinggame.solo.easy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/dwarfs-standing-on-the-shoulders-of-giants
class DwarfStandingTest extends AnyFunSuite with Matchers {

  import DwarfStanding._
  test(" ") {
    val in = List(
      "8",
      "1 2",
      "1 3",
      "3 4",
      "2 4",
      "2 5",
      "10 11",
      "10 1",
      "10 3")

    solve(in) shouldBe 4
  }


}
