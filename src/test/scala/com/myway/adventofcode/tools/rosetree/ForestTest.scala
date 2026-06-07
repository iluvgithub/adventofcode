package com.myway.adventofcode.tools.rosetree

import com.myway.adventofcode.tools.rosetree.Forest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ForestTest extends AnyFunSuite with Matchers {

  val forest: Forest[Char] = branch(
    single('a') :: (
      'b',
      branch(
        List(single('e'), single('f')) ++ (single('d') :: Nil)
      )
    ) :: Nil
  )
  test(" trace") {
    forest.trace shouldBe "a,b(e,f,d)"
  }

  test("browseDepth") {
    forest.browseDepth shouldBe List('a', 'b', 'e', 'f', 'd')
  }

  test("insert string into forest of chars") {
    val actual = List("car", "cat", "z").foldLeft(Forest.empty[Char])(
      Forest.insertInto
    )
    actual.trace shouldBe "z,c(a(t,r))"
  }

}

class ForestZipperTest extends AnyFunSuite with Matchers {

  test("build ") {
    val zip0: ForestZipper[Char] = Forest.initZipper
    val zip1                     = zip0.prepend('d').prepend('c').prepend('b')
    val optZip                   = zip1.downBy('c').map(_.prepend('f').prepend('e'))

    optZip.isDefined shouldBe true
    val zip = optZip.get

    val f = zip.upRoot.focus

    f.trace shouldBe "b,c(e,f),d"

    val zipc = zip.upRoot.downBy('c').get
    zipc.getFocusValue.get shouldBe 'c'

    zipc.setValue('x').upRoot.focus.trace shouldBe "b,x(e,f),d"
  }

}
