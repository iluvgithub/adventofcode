package com.myway.adventofcode.tools.free

import com.myway.adventofcode.tools.free.Free.Trampoline
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FreeTest extends AnyFunSuite with Matchers {

  test("mass sum ") {
    // arrange
    import Free.Bouncer
    def sum(i: Int): Long = recSum(i, 0L).run
    def recSum(i: Int, l: Long): Trampoline[Long] =
      if (i <= 0) Free.done(l)
      else Free.more(() => recSum(i - 1, l + i))
    val n = 30000
    // act
    val out = sum(n)
    // assert
    out shouldBe 1L * n * (n + 1) / 2
  }

}
