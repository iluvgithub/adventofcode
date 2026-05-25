package com.myway.codinggame.solo

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// https://www.codingame.com/ide/puzzle/stock-exchange-losses
class StockExchangePnlTest extends AnyFunSuite with Matchers {

  val in = List(3, 2, 4, 2, 1, 5)

  test("read solve") {
    StockExchangePnl.solve(in) shouldBe -3
  }

}
