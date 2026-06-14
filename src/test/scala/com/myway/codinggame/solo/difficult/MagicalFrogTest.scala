package com.myway.codinggame.solo.difficult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/magical-frog
class MagicalFrogTest extends AnyFunSuite with Matchers {

  import MagicalFrog._
  test("all ") {
    solve(3, 2) shouldBe 3
  }

  test("all +") {
    solve(100, 16) shouldBe 254592179
  }

  test("++") {
    solve(10000, 20) shouldBe 749072746
  }
  test(" k==n") {
    solve(1, 1) shouldBe 1
    solve(2, 2) shouldBe 2
  }


  test(" mass") {
   // solve(1000000000, 20) shouldBe 503933661
  }
}
