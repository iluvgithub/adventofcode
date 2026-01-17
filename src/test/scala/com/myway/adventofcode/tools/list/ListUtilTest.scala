package com.myway.adventofcode.tools.list

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ListUtilTest extends AnyFunSuite with Matchers {

  test(" cpl cpt") {
    ListUtil.cpl('A', List(1, 2, 3)) shouldBe List(('A', 1), ('A', 2), ('A', 3))
    ListUtil.cpr(List(1, 2, 3), 'A') shouldBe List((1, 'A'), (2, 'A'), (3, 'A'))
  }
  test(" cpp") {
    ListUtil.cpp(List('a', 'b', 'c'), List('0', '1')) shouldBe
      List(('a', '0'), ('a', '1'), ('b', '0'), ('b', '1'), ('c', '0'), ('c', '1'))
  }
  test(" cpList") {
    // arrange
    val in = List(List('a', 'b', 'c'), List('-'), List('0', '1'))
    // act
    val out = ListUtil.cpList(in)
    // assert
    out shouldBe List(
      List('a', '-', '0'),
      List('a', '-', '1'),
      List('b', '-', '0'),
      List('b', '-', '1'),
      List('c', '-', '0'),
      List('c', '-', '1')
    )
  }
}
