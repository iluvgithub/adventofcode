package com.myway.codinggame.solo.difficult.variant

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TheResistanceTest extends AnyFunSuite with Matchers {

  test(" more EAI") {
    TheResistance.wordToMorse("EAI") shouldBe "..-.."
  }

  val l = List("......-...-..---.-----.-..-..-..", "5", "HELL", "HELLO", "OWORLD", "WORLD", "TEST")

  test(" solve a") {
    TheResistance.solve("......-...-.." :: l.tail) shouldBe 1
    TheResistance.solve("---.-----.-..-..-.." :: l.tail) shouldBe 1
    TheResistance.solve("......-...-..---" :: l.tail) shouldBe 1
    TheResistance.solve(".-----.-..-..-.." :: l.tail) shouldBe 1
  }

  test(" solve a else") {
    TheResistance.solve(l) shouldBe 2
  }

  val in = List("-.-", "6", "A", "B", "C", "HELLO", "K", "WORLD")

  test(" solve b") {
    TheResistance.solve(in) shouldBe 1
  }

  test("solve 3") {
    TheResistance.solve(
      List(
        "-.-..---.-..---.-..--",
        "5",
        "CAT",
        "KIM",
        "TEXT",
        "TREM",
        "CEM"
      )
    ) shouldBe 125
  }

  test("solve 0") {
    TheResistance.solve(List("", "2", "E", "I")) shouldBe 1L
  }

  test("solve 1") {
    TheResistance.solve(List(".", "1", "E")) shouldBe 1L
  }
  test("solve 5") {
    TheResistance.solve(List("....", "2", "E", "I")) shouldBe 5
  }

  test("solve 6") {
    TheResistance.solve(List(".....", "2", "E", "I")) shouldBe 8
  }

  test("solve 15") {
    TheResistance.solve(List("...............", "2", "E", "I")) shouldBe 987
  }

  test("solve 16") {
    TheResistance.solve(List("................", "2", "E", "I")) shouldBe 1597
  }
}
