package com.myway.adventofcode.tools.monoid

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MonoidTest extends AnyFunSuite with Matchers {

  test(" toBits ") {
    MonoidSyntax.toBits(7) shouldBe List(true, true, true)
    MonoidSyntax.toBits(6) shouldBe List(true, true, false)
    MonoidSyntax.toBits(10) shouldBe List(true, false, true, false)
    MonoidSyntax.toBits(1024) shouldBe List(true, false, false, false, false, false, false, false,
      false, false, false)
  }
  test(" power ") {
    // arrange
    val o = 2L
    val n = 10
    MonoidSyntax.longMultMonoid.power(o, n) shouldBe 1024L

    MonoidSyntax.longMultMonoid.power(o, 9) shouldBe 512L
    MonoidSyntax.longMultMonoid.power(o, 11) shouldBe 2048L
    MonoidSyntax.longMultMonoid.power(o, 12) shouldBe 4096L

    // assert
  }

}
