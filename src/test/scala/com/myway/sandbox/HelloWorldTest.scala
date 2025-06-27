package com.myway.sandbox

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HelloWorldTest extends AnyFunSuite with Matchers {

  test("greet returns correct greeting") {
    // arrange
    val input = "Scala"
    // act
    val actual = HelloWorld.greet(input)
    // assert
    actual shouldBe "Hello, Scala!"
  }

}
