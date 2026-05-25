package com.myway.codinggame.solo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SandFallTest  extends AnyFunSuite with Matchers {

  test("read solve") {
    SandFall.solve(in) shouldBe exp
  }

  def in: List[String] = List(
    "3 3",
    "4",
    "n 1",
    "e 1",
    "o 1",
    "A 1")

  def exp: List[String] = List(
    "|   |",
    "| A |",
    "|one|",
    "+---+")


  test("read solve 2") {
    val in2 = List(
      "2 2",
      "4",
      "S 0",
      "T 1",
      "u 1",
      "J 1"
    )
    val exp = List(
      "|Ju|",
      "|ST|",
      "+--+"
    )
    SandFall.solve(in2) shouldBe exp
  }

}
