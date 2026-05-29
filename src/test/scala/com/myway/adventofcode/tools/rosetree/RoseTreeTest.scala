package com.myway.adventofcode.tools.rosetree

import com.myway.adventofcode.tools.rosetree.RoseTree.{branch, leaf}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RoseTreeTest extends AnyFunSuite with Matchers {

  val rose: RoseTree[Char] = branch(
    'a',
    leaf('b') :: branch(
      'c',
      branch(
        'd',
        leaf('f') :: leaf('g') :: Nil
      ) :: leaf('e') :: Nil
    ) :: Nil
  )

  test("browse Width") {
    rose.browseWidth shouldBe List('a', 'b', 'c', 'd', 'e', 'f', 'g')
  }

  test("browse Depth") {
    rose.browseDepth shouldBe List('a', 'b', 'c', 'd', 'f', 'g', 'e')
  }

}
