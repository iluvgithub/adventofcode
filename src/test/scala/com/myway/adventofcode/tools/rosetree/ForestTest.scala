package com.myway.adventofcode.tools.rosetree

import com.myway.adventofcode.tools.rosetree.Forest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ForestTest extends AnyFunSuite with Matchers {

  test(" trace") {
// arrange
    val forest: Forest[Char] = branch(
      single('a') :: (
        'b',
        branch(
          List(single('e'), single('f')) ++ (single('d') :: Nil)
        )
      ) :: Nil
    )
    // act && assert
    forest.trace shouldBe "a,b(e,f,d)"
  }

}

class ForestZipperTest extends AnyFunSuite with Matchers {

  test("build ") {
    val zip0: ForestZipper[Char] = Forest.initZipper
    val zip1 = zip0.prepend('d').prepend('c').prepend('b')
    val optZip = zip1.downBy('c').map(_.prepend('f').prepend('e'))

    optZip.isDefined shouldBe true
    val zip = optZip.get

    val f = zip.upRoot.focus

    f.trace shouldBe "b,c(e,f),d"
  }

}