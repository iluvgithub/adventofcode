package com.myway.codinggame.solo.easy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/training/easy/periodic-table-spelling
class PeriodicTableSpellingTest extends AnyFunSuite with Matchers {

  import PeriodicTableSpelling._
  test(" ") {
    solve("Carbon") shouldBe List(
      "CArBON",
      "CaRbON"
    )

  }
}
