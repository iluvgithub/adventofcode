package com.myway.adventofcode.tools.bag
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BagTest extends AnyFunSuite with Matchers {

  test(" add") {
    // arrange
    val bag: Bag[Char] = new Bag()
    // act
    val out = bag.addMany('a', 42L).addMany('b', 32).add('a')
    // assert
    out.get('a') shouldBe 43
    out.get('b') shouldBe 32
    out.get('x') shouldBe 0
    out.total shouldBe 75
  }
  test(" bagMap") {
    // arrange
    val bag: Bag[Char] = new Bag().addMany('a', 42L).addMany('b', 32).add('a')
    // act
    val out0 = bag.map(_ => "Z")
    val out1 = bag.map(_.toUpper)
    // assert
    out0.get("Z") shouldBe 75
    out1.get('A') shouldBe 43
    out1.get('B') shouldBe 32
    out1.get('X') shouldBe 0
  }


  test(" construct from map") {
    // arrange
    val m: Map[Char, Boolean] = Map('a' -> true, 'b' -> false, 'c' -> true)
    // act
    val bag = BagOps.codomain(m)
    // assert
    bag.get(true) shouldBe 2L
    bag.get(false) shouldBe 1L
  }

  test(" bag merge") {
    // arrange
    val bag0: Bag[Char] = new Bag().addMany('a', 42L).addMany('b', 32).add('a')
    val bag1: Bag[Char] = new Bag().addMany('a', 7L).addMany('c', 31)
    // act
    val bag = bag0.merge(bag1)
    // assert
    bag.get('a') shouldBe 50L
    bag.get('b') shouldBe 32L
    bag.get('c') shouldBe 31L
  }
  test(" bag flatmap") {
    // arrange
    val bag0: Bag[Char] = new Bag().addMany('a', 42L).addMany('b', 32)
    val bag1: Bag[Char] = new Bag().addMany('a', 7L).addMany('c', 31)
    val bag: Bag[Bag[Char]] = new Bag().addMany(bag0, 2L).add(bag1)
    // act
    val out = bag.flatMap(identity)
    // assert
    out.get('a') shouldBe 91L
    out.get('b') shouldBe 64L
    out.get('c') shouldBe 31L
  }

  test(" bag cross") {
    // arrange
    val bag0: Bag[Char] = new Bag().addMany('a', 5).addMany('b', 1)
    val bag1: Bag[Char] = new Bag().addMany('A', 2).addMany('B', 2).add('C')
    // act
    val bag = bag0.cross(bag1)
    // assert
    bag.get(('a', 'A')) shouldBe 10
    bag.get(('a', 'B')) shouldBe 10
    bag.get(('b', 'C')) shouldBe 1
  }

  test(" bag ix ") {
    // arrange
    val bag: Bag[(Char, Boolean)] = new Bag().addMany(('a', true), 2).addMany(('b', false), 4).
      add(('c', true)).add(('b', true)).add(('a', false)).add(('a', true))
    // act
    val out: Map[Char, Bag[Boolean]] = BagOps.ix(bag)
    // assert
    out.get('a') shouldBe Some(new Bag().addMany(true, 3).addMany(false, 1))
    out.get('b') shouldBe Some(new Bag().addMany(false, 4).add(true))
    out.get('c') shouldBe Some(new Bag().addMany(true, 1))

    BagOps.ix0(out) shouldBe bag
  }

}

