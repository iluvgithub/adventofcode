package com.myway.codinggame.solo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GhostLegTest extends AnyFunSuite with Matchers {

  val in = List(
    "A  B  C",
    "|  |  |",
    "|--|  |",
    "|  |--|",
    "|  |--|",
    "|  |  |",
    "1  2  3")

  test("read solve") {
    GhostLeg.solve(in) shouldBe List("A2", "B1", "C3")
  }
  val in2 = List(
    "F  E  D  C  B  A",
    "|  |--|  |  |  |",
    "|--|  |--|  |--|",
    "|  |--|  |--|  |",
    "|  |  |  |  |--|",
    "|  |--|  |--|  |",
    "|  |  |--|  |  |",
    "|  |  |--|  |--|",
    "|--|  |  |--|  |",
    "|  |  |--|  |  |",
    "|--|  |  |  |--|",
    "|  |--|  |  |  |",
    "|  |  |--|  |  |",
    "0  1  2  3  4  5")
  test("read solve12") {
    GhostLeg.solve(in2).head shouldBe "F3"
  }

}
