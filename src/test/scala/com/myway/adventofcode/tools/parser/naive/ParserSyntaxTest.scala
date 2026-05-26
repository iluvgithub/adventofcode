package com.myway.adventofcode.tools.parser.naive

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserSyntaxTest extends AnyFunSuite with Matchers {

  import ParserSyntax.Parser._
  test("unit ") {
    unit(42L).run("abc") shouldBe Some((42L, "abc"))
  }

  test("fail ") {
    failure.run("abc") shouldBe None
  }

  test("getchar") {
    getchar.parse("abcd") shouldBe Some('a')
    getchar.run("abcd") shouldBe Some(('a', "bcd"))
    getchar.run("") shouldBe None
    getchar.parse("") shouldBe None
  }

  test("sat") {
    sat(_.isDigit).parse("123") shouldBe Some('1')
    sat(_.isDigit).parseSure("123") shouldBe '1'
    sat(_.isDigit).run("123a") shouldBe Some(('1', "23a"))
    sat(_.isDigit).run("a") shouldBe None
  }

  test("char") {
    char('a').parse("abc") shouldBe Some('a')
    char('a').run("abc") shouldBe Some(('a', "bc"))
    char('a').run("bac") shouldBe None
    char0('a').parse("abc") shouldBe Some(())
  }

  test("or else ") {
    val aorb = char('a').or(char('b'))
    aorb.parse("abc") shouldBe Some('a')
    aorb.parse("bca") shouldBe Some('b')
    aorb.run("abc") shouldBe Some(('a', "bc"))
    aorb.run("ccc") shouldBe None

  }


  test("string") {
    string("abc").parse("abcdef") shouldBe Some("abc")
    string("abc").run("abcdef") shouldBe Some(("abc", "def"))
    string0("abc").parse("abcdef") shouldBe Some(())
    string0("abc").run("abcdef") shouldBe Some(((), "def"))
  }


  test("number") {
    number.parse("123") shouldBe Some(123L)
    number.parse("123456789123456") shouldBe Some(123456789123456L)

    number.parse(" ") shouldBe Some(0)
    number.run(" ") shouldBe Some((0, " "))
    number.parse("") shouldBe Some(0)
    number.parse(" ") shouldBe Some(0)
  }

  test("many ") {
    val m = many(string("ab"))
    m.parse("abcdef") shouldBe Some("ab" :: Nil)
    m.parse("ababc") shouldBe Some("ab" :: "ab" :: Nil)
    m.parse("") shouldBe Some(Nil)

    m.run("ababc") shouldBe Some(("ab" :: "ab" :: Nil, "c"))
  }

  test("lowers") {
      lowers.parse("abcDEFg") shouldBe Some("abc")
  }

}
