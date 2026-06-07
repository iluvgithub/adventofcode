package com.myway.adventofcode.tools.rosetree

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TrieTest extends AnyFunSuite with Matchers {

  import com.myway.adventofcode.tools.monoid.MonoidSyntax.longPlusMonoid
  test(" one char test") {
    val trie0: Trie[Long] = Trie.empty[Long]
    val o: Trie[Long]     = trie0.insert("c", 1L)

    o.search("c") shouldBe Some(1L)
    o.search("b") shouldBe None
  }

  test(" two chars test") {
    val trie0         = Trie.empty[Long]
    val t: Trie[Long] = trie0.insert("ca", 1L)
    val o: Trie[Long] = t.insert("car", 2L)
    val q             = o.insert("ca", 2L)

    o.search("ca") shouldBe Some(1L)
    o.search("cb") shouldBe None
    o.search("car") shouldBe Some(2L)
    q.search("ca") shouldBe Some(3L)
  }

  test(" first test") {
    val trie0         = Trie.empty[Long]
    val o: Trie[Long] = trie0.insert("cat", 1L).insert("car", 2L)

    o.search("cat") shouldBe Some(1L)
    o.search("car") shouldBe Some(2L)
    o.search("cab") shouldBe None
  }

}
