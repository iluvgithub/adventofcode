package com.myway.codinggame.solo.medium

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TiredPacManTest extends AnyFunSuite with Matchers {

  import TiredPacMan._

  test(" go" ) {
    val in = List(
      "12 9",
      "13",
      "############",
      "# .     *  #",
      "# .        #",
      "# .        #",
      "#.         #",
      "# .   )P   #",
      "# .        #",
      "# .       *#",
      "############")

    //solve(in) shouldBe 10L
  }
}
