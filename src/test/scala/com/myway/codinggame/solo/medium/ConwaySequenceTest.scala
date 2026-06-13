package com.myway.codinggame.solo.medium

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/conway-sequence
class ConwaySequenceTest extends AnyFunSuite with Matchers {

  import ConwaySequence._
  test("a ") {
    solve(1, 5) shouldBe "1 1 1 2 2 1"
    solve(1, 6) shouldBe "3 1 2 2 1 1"
    solve(1, 7) shouldBe "1 3 1 1 2 2 2 1"
  }

}
