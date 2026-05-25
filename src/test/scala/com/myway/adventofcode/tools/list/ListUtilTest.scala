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

  test("split") {
    // arrange
    val l = List(1, 2, 3)
    // act
    val out = ListUtil.splits(l)
    // assert
    out shouldBe List(
      (List(), List(1, 2, 3)),
      (List(1), List(2, 3)),
      (List(1, 2), List(3)),
      (List(1, 2, 3), List())
    )
  }

  test("permutation") {
    // arrange
    val l = List(1, 2, 3)
    // act
    val out = ListUtil.permutations(l)
    // assert
    out shouldBe List(
      List(3, 2, 1),
      List(2, 3, 1),
      List(2, 1, 3),
      List(3, 1, 2),
      List(1, 3, 2),
      List(1, 2, 3)
    )
  }

  test("maximum segment sum") {
    // arrange
    val diff  = List(-1, 2, -2, -1, 4)
    val diff1 = List(-1, 2, -2, -1, 4, -1, 3)
    val l = List(-2, 1, -3, 4, -1, 2, 1, -5, 4)

    // act
    val out  = ListUtil.maxSegSum(diff)
    val out1 = ListUtil.maxSegSum(diff1)
    val o    = ListUtil.maxSegSum(l)
    // assert
    out shouldBe List(4)
    out.sum shouldBe 4
    out1 shouldBe List(4, -1, 3)
    out1.sum shouldBe 6

    o shouldBe List(4, -1, 2, 1)
    o.sum shouldBe 6
  }

}
