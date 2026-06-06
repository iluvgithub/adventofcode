package com.myway.adventofcode.tools.rosetree

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TrieTest extends AnyFunSuite with Matchers {

  test(" one char test") {
    val trie0 = new Trie()
    val o: Trie = trie0.insert("c")

    o.search("c") shouldBe true
    o.search("b") shouldBe false
  }

  test(" two chars test") {
    val trie0 = new Trie()
    val o: Trie = trie0.insert("ca")

    o.search("ca") shouldBe true
    o.search("cb") shouldBe false
  }

  test(" first test") {
    val trie0 = new Trie()
    val o: Trie = trie0.insert("cat").insert("car")
 
    o.search("cat") shouldBe true
    o.search("car") shouldBe true
    o.search("cab") shouldBe false
  }

}
