package com.myway.codinggame.solo.easy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/six-degrees-of-kevin-bacon
class SixDegreesKevinBaconTest extends AnyFunSuite with Matchers {

  import SixDegreesKevinBacon._
  test(" ok") {
    solve(List(
      "Elvis Presley",
      "3",
      "Change of Habit: Elvis Presley, Mary Tyler Moore, Barbara McNair, Jane Elliot, Ed Asner",
      "JFK: Kevin Costner, Kevin Bacon, Tommy Lee Jones, Laurie Metcalf, Gary Oldman, Ed Asner",
      "Sleepers: Kevin Bacon, Jason Patric, Brad Pitt, Robert De Niro, Dustin Hoffman")
    ) shouldBe 2
  }
}
