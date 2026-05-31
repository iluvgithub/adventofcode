package com.myway.codinggame.solo.difficult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/the-resistance
class TheResistanceTest extends AnyFunSuite with Matchers {


  val l = List(
    "......-...-..---.-----.-..-..-..",
    "5",
    "HELL",
    "HELLO",
    "OWORLD",
    "WORLD",
    "TEST")

  test(" solve a")  {
    TheResistance.solve(l) shouldBe 2
  }

  val in = List(
    "-.-",
    "6",
    "A",
    "B",
    "C",
    "HELLO",
    "K",
    "WORLD")

  test(" solve b")  {
    TheResistance.solve(in) shouldBe 1
  }


  test ("solve 3") {
    TheResistance.solve(List(
      "-.-..---.-..---.-..--",
      "5",
      "CAT",
      "KIM",
      "TEXT",
      "TREM",
      "CEM"
    ) ) shouldBe 125
  }


  test ("solve 4") {
    TheResistance.solve(List(
      "...",
      "2",
      "E",
      "I")) shouldBe 3
  }


  test ("solve 5") {
    TheResistance.solve(List(
      "....",
      "2",
      "E",
      "I")) shouldBe 5
  }

  test ("solve 6") {
    TheResistance.solve(List(
      ".....",
      "2",
      "E",
      "I")) shouldBe 8
  }
}
