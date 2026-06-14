package com.myway.adventofcode.tools.category

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FunctorTest extends AnyFunSuite with Matchers {

  test(" for yield with map") {
    // arrange
    case class Wrapper[A](a: A)
    implicit val wrapFunctor: Functor[Wrapper] = new Functor[Wrapper] {
      override def map[A, B](fa: Wrapper[A], f: A => B): Wrapper[B] =
        Wrapper(f(fa.a))
    }
    val w              = new Wrapper[Int](3)
    val f: Int => Long = i => 39L + i
    // act
    import FunctorObj._
    val out = for {
      x <- w
      z = f(x)
    } yield z
    // assert
    out.a shouldBe 42L
  }

  test(" for yield with map and filter") {
    // arrange
    case class WrapperFilter[A](o: Option[A]) {
      def this(a: A) = this(Some(a))
    }
    implicit val wrapFunctor: Functor[WrapperFilter] = new Functor[WrapperFilter] {
      override def map[A, B](fa: WrapperFilter[A], f: A => B): WrapperFilter[B] =
        WrapperFilter(fa.o.map(f))
    }
    implicit val wrapFilterFunctor: Filterable[WrapperFilter] = new Filterable[WrapperFilter] {
      override def withFilter[A](fa: WrapperFilter[A], p: A => Boolean): WrapperFilter[A] =
        if (fa.o.exists(p)) fa else WrapperFilter[A](None)
    }

    val w = new WrapperFilter[Int](-3)
    val f:Int=>Int = _ * 2
    // act
    import FilterableObj._
    val out = for {
      x <- w
      if x > 0
    } yield f(x)
    // assert
    out.o shouldBe None
  }

}
