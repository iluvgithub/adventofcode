package com.myway.codinggame.solo

import com.myway.codinggame.solo.FindConsecutive.solve
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/abcdefghijklmnopqrstuvwxyz
class FindConsecutiveTest  extends AnyFunSuite with Matchers {
  val in = List(
    "10",
    "qadnhwbnyw",
    "iiopcygydk",
    "bahlfiojdc",
    "cfijtdmkgf",
    "dzhkliplzg",
    "efgrmpqryx",
    "loehnovstw",
    "jrsOcymeuv",
    "fpnocpdkrs",
    "jlmsvwvuih")


  val expected =  List(
    "----------",
    "----------",
    "ba--------",
    "c-ij------",
    "d-hkl---z-",
    "efg-mpqryx",
    "----no-stw",
    "--------uv",
    "----------",
    "----------")
  test("read solve") {
    val out = solve(in)


    out shouldBe expected
  }

  val in0 = List(
    "10",
    "vkbjbzmbgb",
    "abcccpzouv",
    "fedopwlmcl",
    "glmnqrszyw",
    "hkrhiutymj",
    "ijqcmvwxoc",
    "pcvlpqzphl",
    "hsgvoklcxy",
    "urdjusmbmz",
    "rchbcausnp")


  val expected0 = List(
    "----------",
    "abc-------",
    "fedop-----",
    "glmnqrsz--",
    "hk---uty--",
    "ij---vwx--",
    "----------",
    "----------",
    "----------",
    "----------")

  test("read solve 0") {
    val out = solve(in0)

    out shouldBe expected0
  }
}
