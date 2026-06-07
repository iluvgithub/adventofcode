package com.myway.adventofcode.tools.monoid

trait Monoid[X] {

  def neutral: X

  def add(left: X, right: X): X

}

object MonoidSyntax {

  @inline final def apply[A](implicit ev: Monoid[A]): Monoid[A] = ev

  implicit val intPlusMonoid: Monoid[Int] = new Monoid[Int] {
    override def neutral: Int = 0

    override def add(left: Int, right: Int): Int = left + right
  }

  implicit val intMultMonoid: Monoid[Int] = new Monoid[Int] {
    override def neutral: Int = 1

    override def add(left: Int, right: Int): Int = left * right
  }

  implicit val longPlusMonoid: Monoid[Long] = new Monoid[Long] {
    override def neutral: Long = 0L

    override def add(left: Long, right: Long): Long = left + right
  }

  implicit val longMultMonoid: Monoid[Long] = new Monoid[Long] {
    override def neutral: Long = 1L

    override def add(left: Long, right: Long): Long = left * right
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def neutral: String = ""

    override def add(left: String, right: String): String = left ++ right
  }
}
