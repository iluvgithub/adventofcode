package com.myway.codinggame.solo

import com.myway.codinggame.solo.RectangularBlockSpinner._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RectangularBlockSpinnerTest extends AnyFunSuite with Matchers {

  val in = List("5", "45",
    "# # # # #",
    "# - . . #",
    "# # - . #",
    "# # # - #",
    "# # # # #")

  val expected = List(
    "    #    ",
    "   # #   ",
    "  # . #  ",
    " # . . # ",
    "# - - - #",
    " # # # # ",
    "  # # #  ",
    "   # #   ",
    "    #    "
  )

  test("read solve") {
    val out = solve(in)

    out shouldBe expected
  }

  val in2 = List("3", "45",
    "ABC",
    "DEF",
    "GHI")

  val expected2 = List(
    "  C  ",
    " B F ",
    "A E I",
    " D H ",
    "  G  ")

  test("read solve 2" ) {
    import RectangularBlockSpinner._
    val out = solve(in2)

    out shouldBe expected2
  }


  test("read transpose 2" ) {
    import RectangularBlockSpinner._
    val out = transpose(in2.tail.tail)

    out shouldBe List(
    "CFI",
    "BEH",
    "ADG")
  }

  val in3 = List("2", "45",
    "UV",
    "WX",
    )
  val expected3 = List(
    " V ",
    "U X",
    " W ")

  test("read solve 3" ) {
    import RectangularBlockSpinner._
    val out = solve(in3)

    out shouldBe expected3
  }


  val in4=      List(
    "16",
    "315",
    "G W A",
    "T B Z",
    "C P E")

  val expected4  = List(
    "  G  ",
    " T W ",
    "C B A",
    " P Z ",
    "  E  ")

  test("read solve4 " ) {
    import RectangularBlockSpinner._
    val out = solve(in4)

    out shouldBe expected4
  }

}
