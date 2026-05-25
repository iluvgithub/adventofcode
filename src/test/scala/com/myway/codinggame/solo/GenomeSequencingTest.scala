package com.myway.codinggame.solo

import com.myway.codinggame.solo.GenomeSequencing._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
// https://www.codingame.com/ide/puzzle/genome-sequencing
class GenomeSequencingTest extends AnyFunSuite with Matchers {

  val in = List("AGATTA", "GATTACA", "TACAGA")

  test("read solve") {

    solve(in).size shouldBe "AGATTACAGA".size
  }

  test("cat") {
    val l = "abc"
    val r = "cd"
    cat(l, r) shouldBe "abcd"
    cat(r, l) shouldBe "cdabc"

    cat("abcdef", "defghij") shouldBe "abcdefghij"

    cat("AGATTA", "GAT") shouldBe "AGATTA"
  }
}
