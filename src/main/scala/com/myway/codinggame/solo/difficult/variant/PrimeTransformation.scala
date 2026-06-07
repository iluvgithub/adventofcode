package com.myway.codinggame.solo.difficult.variant

object PrimeTransformation {

  def solve(l: List[String]): Long = {
    val number            = l.head.toLong
    val primes: List[Int] = generate(number)

    val mapping: Map[Int, Int] =
      l.tail.tail.map(_.split(" ")).map(arr => arr(0).toInt -> arr(1).toInt).toMap

    primes.map(k => mapping.getOrElse(k, 0)).
      foldLeft(1L)((acc, x) => acc * x)
  }

  import scala.collection.mutable.ListBuffer

  def generate(n: Long): List[Int] = {
    val primes    = ListBuffer[Int]()
    var num       = n
    var candidate = 2

    while (num > 1) {
      while (num % candidate == 0) {
        primes += candidate
        num /= candidate
      }
      candidate += 1
    }

    primes.toList
  }
}
