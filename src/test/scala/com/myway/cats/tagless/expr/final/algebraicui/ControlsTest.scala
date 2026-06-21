package com.myway.cats.tagless.expr.`final`.algebraicui

import com.myway.cats.tagless.expr.`final`.algebraicui.SimpleType.Program
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ControlsTest extends AnyFunSuite with Matchers {


  test("control  ") {
    // arrange
    val inputs = "John Doo" :: "42" :: "0" :: Nil
    var idx = 0
    val reader: () => String = () => {
      val out = inputs(idx)
      idx = idx + 1
      out
    }
    var output: List[String] = Nil
    val printer: String => Unit = s => {
      output ++ List(s)
    }
    val inOutSimple: Simple = new Simple(printer, reader)

    val program: Program[(String, Int)] = Program.quiz[Program](inOutSimple, inOutSimple)
    // act

    val (name, rating) = program()
    // assert
    name shouldBe inputs.head
    rating shouldBe inputs(2).toInt + 1

  }

}
