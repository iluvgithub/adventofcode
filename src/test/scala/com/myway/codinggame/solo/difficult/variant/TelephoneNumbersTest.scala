package com.myway.codinggame.solo.difficult.variant

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TelephoneNumbersTest extends AnyFunSuite with Matchers {

  test(" a00") {
    TelephoneNumbers.solve(
      "1" :: "00" :: Nil
    ) shouldBe 2L
  }

  test(" a") {
    TelephoneNumbers.solve(
      "1" :: "0467123456" :: Nil
    ) shouldBe 10L
  }

  test(" b") {
    TelephoneNumbers.solve(
      "2" :: "0123456789" :: "1123456789" :: Nil
    ) shouldBe 20L
  }

  test(" c") {
    TelephoneNumbers.solve(
      "2" :: "0123456789" :: "0123" :: Nil
    ) shouldBe 10L
  }

  test(" d") {
    val in = List("5", "0412578440", "0412199803", "0468892011", "112", "15")
    TelephoneNumbers.solve(in) shouldBe 28L
  }

}
