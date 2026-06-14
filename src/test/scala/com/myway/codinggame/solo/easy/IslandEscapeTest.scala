package com.myway.codinggame.solo.easy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/island-escape
class IslandEscapeTest extends AnyFunSuite with Matchers {

  test(" no escape") {
    val in = List("3", "0 0 0", "0 2 0", "0 0 0")
    IslandEscape.solve(in) shouldBe "no"
  }

  test("   escape") {
    val in = List("3", "0 1 0", "0 2 0", "0 0 0")
    IslandEscape.solve(in) shouldBe "yes"
  }
}
