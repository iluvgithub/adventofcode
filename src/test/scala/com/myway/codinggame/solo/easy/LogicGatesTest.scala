package com.myway.codinggame.solo.easy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/logic-gates
class LogicGatesTest extends AnyFunSuite with Matchers {

  val in = List(
    "2",
    "3",
    "A __---___---___---___---___",
    "B ____---___---___---___---_",
    "C AND A B",
    "D OR A B",
    "E XOR A B"
  )

  test(" ") {

    val actual = LogicGates.solve(in)
    val expected = List(
      "C ____-_____-_____-_____-___",
      "D __-----_-----_-----_-----_",
      "E __--_--_--_--_--_--_--_--_"
    )

    actual shouldBe expected


  }

}
