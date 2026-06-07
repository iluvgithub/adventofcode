package com.myway.adventofcode.tools.rosetree

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BinTreeTest extends AnyFunSuite with Matchers  {
  import BinTree.leaf
  test(" build and trace") {

    val z: BinTreeZipper[Char] = new BinTreeZipper( leaf('a'))
    val newBin = z.extendLeft(leaf('b')).extendRight(leaf('c')).right.map(_.extendLeft(leaf('d'))).map(_.upRoot).get.focus
    newBin.trace shouldBe "a[b,c[d,]]"

    val z1: BinTreeZipper[Char] = new BinTreeZipper( newBin)
    val z2 = z1.left.map(_.setOnFocus('x')).map(_.extendRight(leaf('y'))).get.upRoot

    z2.focus.trace shouldBe "a[x[,y],c[d,]]"

  }

}
