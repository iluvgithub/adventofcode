package com.myway.codinggame.solo

import com.myway.codinggame.solo.JetCoaster._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JetCoasterTest extends AnyFunSuite with Matchers {

  test(" battle") {
    // arrange
    val l    = 3
    val c    = 3
    val n    = 4
    val list = List(3, 1, 1, 2)
    // act
    val out = solve(l, c, n, list)
    // assert
    out shouldBe 7
  }

  test(" battle mass") {
    // arrange
    val l    = 10000
    val c    = 10
    val n    = 5
    val list = List(100, 200, 300, 400, 500)
    // act
    val out = solve(l, c, n, list)
    // assert
    out shouldBe 15000
  }

}
