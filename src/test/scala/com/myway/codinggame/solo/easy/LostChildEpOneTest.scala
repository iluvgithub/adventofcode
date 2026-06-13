package com.myway.codinggame.solo.easy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/the-lost-child-episode-1
class LostChildEpOneTest extends AnyFunSuite with Matchers {

  import LostChildEpOne._
  test ( " " ) {

    val in = List(
    "..........",
    "M....C....",
    "..........",
    "..........",
    "..........",
    "..........",
    "..........",
    "..........",
    "..........",
    "..........")

    solve(in) shouldBe "50km"
  }


}
