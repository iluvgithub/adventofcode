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
          List(single('e'),
            single('f')
          ) ++ (single('d') :: Nil)
        )
      ) :: Nil
    )
    // act && assert
    forest.trace shouldBe "a,b(e,f,d)"
  }

}
