package com.myway.codinggame.solo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PlayingCardOddsTest extends AnyFunSuite with Matchers {

  import PlayingCardOdds._
  test(" odds compute") {
    solve("45C" :: Nil, "7H" :: Nil) shouldBe "2%"

  }

  test("full") {
    val removed = List("CH", "23456789", "TDS")
    val sought  = List("ADS", "JQK")
    solve( removed, sought) shouldBe "100%"
  }

  test("mixture") {
    val removed = List("234C", "567H", "89TCH", "JQKD", "25JS", "369D", "A")
    val sought  = List( "JQKAS", "2469H", "4TCD")
    solve( removed, sought) shouldBe "22%"
  }


  test(" odds") {
    extractRanks("") shouldBe List()
    extractRanks("7") shouldBe List('7')
    extractRanks("7T") shouldBe List('7', 'T')

    extractColors("D") shouldBe List('D')
    extractColors("SCHD") shouldBe List('S', 'C', 'H', 'D')
  }

  test(" actual Cards ") {
    actualCards("") shouldBe Nil
    actualCards("7H") shouldBe List(Card('7', 'H'))
    actualCards("7HD") shouldBe List(Card('7', 'H'), Card('7', 'D'))
    actualCards("57H") shouldBe List(Card('5', 'H'), Card('7', 'H'))
    actualCards("7") shouldBe List(Card('7', 'C'), Card('7', 'D'), Card('7', 'H'), Card('7', 'S'))

    actualCards("H") shouldBe ranks.map(x => Card(x, 'H'))
  }

  test(" actual Cards 2") {
    actualCards("7") shouldBe List(Card('7', 'C'), Card('7', 'D'), Card('7', 'H'), Card('7', 'S'))

  }
}
