package com.myway.adventofcode.tools.monoid

trait Monoid[X] {

  def neutral: X

  def add(left: X, right: X): X

  def power(x: X, n: Int) = MonoidSyntax
    .toBits(n).reverse
    .foldLeft((neutral, x))((acc, b) => (if (b) add(acc._2, acc._1) else acc._1, add(acc._2, acc._2)))
    ._1
}

object MonoidSyntax {

  import scala.annotation.tailrec
  def toBits(n: Int): List[Boolean] = {
    @tailrec
    def loop(x: Int, acc: List[Boolean]): List[Boolean] =
      if (x <= 0) acc
      else loop(x / 2, (x % 2 > 0) :: acc)

    loop(n, Nil)
  }

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
