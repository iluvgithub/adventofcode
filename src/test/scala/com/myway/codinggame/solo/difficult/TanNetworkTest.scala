package com.myway.codinggame.solo.difficult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/tan-network
class TanNetworkTest extends AnyFunSuite with Matchers {

  import TanNetwork._

  test("  ") {
    val in = List(
      "StopArea:ABDU",
      "StopArea:ABLA",
      "3",
      "StopArea:ABDU,\"Abel Durand\",,47.22019661,-1.60337553,,,1,",
      "StopArea:ABLA,\"Avenue Blanche\",,47.22973509,-1.58937990,,,1,",
      "StopArea:ACHA,\"Angle Chaillou\",,47.26979248,-1.57206627,,,1,",
      "2",
      "StopArea:ABDU StopArea:ABLA",
      "StopArea:ABLA StopArea:ACHA"
    )

    solve(in) shouldBe List(
      "Abel Durand",
      "Avenue Blanche"
    )
  }

}
