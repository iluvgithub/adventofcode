package com.myway.adventofcode.tools.category

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FunctorTest extends AnyFunSuite with Matchers {

  test( " for yield") {
    // arrange
    case class Wrapper[A](a:A)
    implicit val wrapFunctor = new Functor[Wrapper] {
      override def map[A, B](fa: Wrapper[A], f: A => B): Wrapper[B] =
        Wrapper(f(fa.a))
    }
    val w = new Wrapper[Int](3)
    val f:Int=>Long =i=> 39L +i
    // act
    import FunctorObj._
    val out = for {
      x <- w
      z = f(x)
    } yield z
    // assert
    out.a shouldBe 42L
  }


}
