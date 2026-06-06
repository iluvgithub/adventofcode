package com.myway.codinggame.solo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/minicpu-instruction-decoder
class MinicpuInstructionDecoderTest extends AnyFunSuite with Matchers {

  test(" first test") {
    // arrange
    val in = "01 00 0A 01 01 05 02 00 01 03 00 01 FF"
    // act
    val out = MiniCpuInstructionDecoder.solve(in)
    // assert
    out shouldBe List(10, 5, 0, 0)
  }
  test(" FF") {
    // arrange
    val in = "FF"
    // act
    val out = MiniCpuInstructionDecoder.solve(in)
    // assert
    out shouldBe List(0, 0, 0, 0)
  }

  test(" inc overflow") {
    // arrange
    val in = "01 00 FF 05 00 FF"
    // act
    val out = MiniCpuInstructionDecoder.solve(in)
    // assert
    out shouldBe List(0, 0, 0, 0)
  }
  test(" dec overflow") {
    // arrange
    val in = "01 03 00 06 03 FF"
    // act
    val out = MiniCpuInstructionDecoder.solve(in)
    // assert
    out shouldBe List(0, 0, 0, 255)
  }

}
