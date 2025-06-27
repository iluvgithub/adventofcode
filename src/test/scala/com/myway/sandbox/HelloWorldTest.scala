package com.myway.sandbox

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HelloWorldTest extends AnyFunSuite with Matchers {

  test("greet returns correct greeting") {
    val result = HelloWorld.greet("Scala")
    assert(result == "Hello, Scala!")
    HelloWorld.greet("Scala") shouldBe "Hello, Scala!"
  }
}
