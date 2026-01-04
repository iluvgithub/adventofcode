package com.myway.adventofcode.adv2025

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day11Test extends AnyFunSuite with Matchers {

  val in = List(
    "aaa: you hhh",
    "you: bbb ccc",
    "bbb: ddd eee",
    "ccc: ddd eee fff",
    "ddd: ggg",
    "eee: out",
    "fff: out",
    "ggg: out",
    "hhh: ccc fff iii",
    "iii: out")

  test("year:25 day:11 case 1") {
    // arrange

    // act
    val out = Day11.solve1(in)
    // assert
    out shouldBe 5L
  }


  val in2 = List(
    "svr: aaa bbb",
    "aaa: fft",
    "fft: ccc",
    "bbb: tty",
    "tty: ccc",
    "ccc: ddd eee",
    "ddd: hub",
    "hub: fff",
    "eee: dac",
    "dac: fff",
    "fff: ggg hhh",
    "ggg: out",
    "hhh: out")
  test("year:25 day:11 case 2") {
    // arrange
    // act
    val out = Day11.solve2(in2)
    // assert
    out shouldBe 2L
  }

}
