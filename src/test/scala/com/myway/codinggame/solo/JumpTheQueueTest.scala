package com.myway.codinggame.solo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
// https://www.codingame.com/ide/puzzle/jump-the-queue
class JumpTheQueueTest  extends AnyFunSuite with Matchers {

  import JumpTheQueue._
  val l: List[String] = List(
    "2 14",
    "111 112 113",
    "221 222 223",
    "111 222 333 112 223 113 221 -1 -1 -1 -1 -1 -1 -1")

  test(" handle queue") {
    solve(l) shouldBe List(
      "111",
      "112",
      "113",
      "222",
      "223",
      "221",
      "333")
  }

}
