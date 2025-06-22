package com.myway.sandbox

import org.scalatest.funsuite.AnyFunSuite

class HelloWorldTest extends AnyFunSuite {

  test("greet returns correct greeting") {
    val result = HelloWorld.greet("Scala")
    assert(result == "Hello, Scala!")
  }
}
