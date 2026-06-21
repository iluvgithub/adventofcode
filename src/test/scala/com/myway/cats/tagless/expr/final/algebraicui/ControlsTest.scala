package com.myway.cats.tagless.expr.`final`.algebraicui

import com.myway.cats.tagless.expr.`final`.algebraicui.SimpleType.Program
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ControlsTest extends AnyFunSuite with Matchers {


  test("control  ") {
    // arrange
    val inputs = "John Doo" :: "42" :: "3" :: Nil
    var idx = 0
    val reader: () => String = () => {
      val out = inputs(idx)
      idx = idx + 1
      out
    }
    var output: List[String] = Nil
    val printer: String => Unit = s => {
      output = output ++ List(s)
    }
    val inOutSimple: Simple = Simple(printer, reader)

    val program: Program[(String, Int)] = Program.quiz[Program](inOutSimple, inOutSimple)
    // act

    val (name, rating) = program()
    // assert
    name shouldBe inputs.head
    rating shouldBe inputs(2).toInt
    output shouldBe List(
      "What is your name? (e.g. John Doe):",
      "Tagless final is the greatest thing ever",
      "1: Strongly disagree",
      "2: Disagree",
      "3: Neutral",
      "4: Agree",
      "5: Strongly agree",
      "Please enter a valid number.",
      "Tagless final is the greatest thing ever",
      "1: Strongly disagree",
      "2: Disagree",
      "3: Neutral",
      "4: Agree",
      "5: Strongly agree")


  }

}
