package com.myway.codinggame.solo.difficult.variant

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/prime-transformations
class PrimeTransformationTest extends AnyFunSuite with Matchers {

  import PrimeTransformation._
  test(" one ") {

    val in = List(
      "16",
      "1",
      "2 5"
    )

    solve(in) shouldBe 625L

  }

}
