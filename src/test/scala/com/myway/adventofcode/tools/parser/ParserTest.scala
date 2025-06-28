package com.myway.adventofcode.tools.parser

import com.myway.adventofcode.tools.parser.ParserErrorMonad.ERROR_PARSER_MON.unit
import com.myway.adventofcode.tools.parser.ParserErrorMonad.{char, char0, digit, getchar, many, number, orElse, sat, string, string0, token}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFunSuite with Matchers {

  import ParserErrorMonad.ParserErrorWrapper

  test("unit ") {
    unit(42L).run("abc") shouldBe Right(42L, "abc")
  }
  test("fail ") {
    ParserErrorMonad.fail("message").run("abc") shouldBe Left("message")
  }
  test("getchar") {
    getchar.parse("abcd") shouldBe Right('a')
    getchar.run("abcd") shouldBe Right(('a', "bcd"))
    getchar.run("") shouldBe Left("Empty String")
    getchar.parse("") shouldBe Left("Empty String")
  }

  test("sat") {
    sat(_.isDigit).parse("123") shouldBe Right('1')
    sat(_.isDigit).run("123a") shouldBe Right(('1', "23a"))
    sat(_.isDigit).run("a") shouldBe Left("[a] failed test")
  }


  test("char") {
    char('a').parse("abc") shouldBe Right('a')
    char('a').run("abc") shouldBe Right(('a', "bc"))
    char('a').run("bac") shouldBe Left("[b] failed test")
    char0('a').parse("abc") shouldBe Right(())
  }
  test("or else ") {
    val aorb = orElse(char('a'), char('b'))
    aorb.parse("abc") shouldBe Right('a')
    aorb.parse("bca") shouldBe Right('b')
    aorb.run("abc") shouldBe Right(('a', "bc"))
    aorb.run("ccc") shouldBe Left("[c] failed test")

  }

  test("string") {
    string("abc").parse("abcdef") shouldBe Right("abc")
    string("abc").run("abcdef") shouldBe Right(("abc", "def"))
    string0("abc").parse("abcdef") shouldBe Right(())
    string0("abc").run("abcdef") shouldBe Right(((), "def"))
  }


  test("string token") {
    token(string("abc")).parse("     abcdef") shouldBe Right("abc")
    token(string("abc")).run("abcdef") shouldBe Right(("abc", "def"))
    token(string("abc")).run(" abcdef") shouldBe Right(("abc", "def"))
    token(string("abc")).run("   abcdef") shouldBe Right(("abc", "def"))
  }

  test("number") {
    digit.parse("123") shouldBe Right(1)
    digit.run("456") shouldBe Right((4, "56"))
    number.parse("123") shouldBe Right(123L)
    number.parse("123456789123456") shouldBe Right(123456789123456L)
  }

  test("many ") {
    val m = many(string("ab"))
    m.parse("abcdef") shouldBe Right("ab" :: Nil)
    m.parse("ababc") shouldBe Right("ab" :: "ab" :: Nil)
    m.parse("") shouldBe Right(Nil)

    m.run("ababc") shouldBe Right(("ab" :: "ab" :: Nil, "c"))
  }

}
