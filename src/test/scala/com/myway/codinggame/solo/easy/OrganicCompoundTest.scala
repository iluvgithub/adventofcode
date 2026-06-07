package com.myway.codinggame.solo.easy

import com.myway.codinggame.solo.easy.OrganicCompound._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
// https://www.codingame.com/ide/puzzle/organic-compounds
class OrganicCompoundTest extends AnyFunSuite with Matchers {

  test(" ok") {
    val l = List(
      "1",
      "CH3(1)CH2(1)CH3"
    )

    Molecule(CarboUnit(3), (1, CarboUnit(3)) :: Nil).isValid shouldBe true
    solve(l) shouldBe "VALID"
  }

  test(" ko") {
    val l = List(
      "1",
      "CH3(1)CH1(1)CH3"
    )
    solve(l) shouldBe "INVALID"
  }

  test(" ok one") {
    val l = List(
      "1",
      "CH4"
    )
    solve(l) shouldBe "VALID"
  }

  test("not ok one") {
    Molecule(CarboUnit(4), Nil).isValid shouldBe true
    Molecule(CarboUnit(3), Nil).isValid shouldBe false
    val l = List(
      "1",
      "CH3"
    )
    OrganicCompound.parseCarbo.parse("CH3") shouldBe Some(CarboUnit(3))
    OrganicCompound.molParser.parse("CH3") shouldBe Some(Molecule(CarboUnit(3), Nil))
    OrganicCompound.isValid("CH3") shouldBe false
    OrganicCompound.solve(l) shouldBe "INVALID"
  }
}
