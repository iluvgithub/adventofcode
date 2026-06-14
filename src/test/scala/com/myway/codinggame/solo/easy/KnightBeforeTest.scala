package com.myway.codinggame.solo.easy

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class KnightBeforeTest extends AnyFunSuite with Matchers {

  import KnightBefore._
  test(" no  ") {

    val in = List(
      "RNBQKP",
      "RNBQKBNR",
      "PPPPPPPP",
      "........",
      "........",
      "........",
      "........",
      "pppppppp",
      "rnbqkbnr",
      "RNBQKBNR",
      "PPPPPPPP",
      "........",
      "........",
      "........",
      "....p...",
      "pppp.ppp",
      "rnbqkbnr"
    )

    solve(in) shouldBe List("e2-e3", "Other")

  }

  test(" knight") {

    val in = List(
      "RNBQKP",
      "R.BQKBNR",
      "PPP.PPPP",
      "..N.....",
      "...P....",
      "....p...",
      "..n.....",
      "pppp.ppp",
      "r.bqkbnr",
      "R.BQKBNR",
      "PPP.PPPP",
      "..N.....",
      "...n....",
      "....p...",
      "........",
      "pppp.ppp",
      "r.bqkbnr")


    solve(in) shouldBe List("c3xd5", "Knight")
  }

  test(" bug") {val in = List(
    "TCFDRP",

    "T1FDRoCT",
    "PPP.x.PP",
    "oxCoxP.x",
    "esFxPx.1",
    "xp*Pp.xo",
    "oxcxop .",
    "poppx.pp",
    "txfdrfct",

    "T1FDRoCT",
    "PPP.x.PP",
    "ox*oxP.x",
    "esFxPx.1",
    "xC*Pp.xo",
    "oxcxop u",
    "poppx.pp",
    "txfdrfct")


    solve(in) shouldBe List("c6xb4", "Knight")
  }

}
