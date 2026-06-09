package com.myway.codinggame.solo.medium

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/the-gift
class TheGiftTest extends AnyFunSuite with Matchers {

  import TheGift._
  test("a ") {
    solve(
      List(
        3, 100, 20, 20, 40
      )
    ) shouldBe Left("IMPOSSIBLE")
  }

  test("b") {
    solve(
      List(
        3, 100, 40, 40, 40
      )
    ) shouldBe Right(List(33, 33, 34))
  }

  test("c") {
    solve(
      List(
        3, 100, 100, 1, 60
      )
    ) shouldBe Right(List(1, 49, 50))
  }

}
