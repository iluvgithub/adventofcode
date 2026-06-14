package com.myway.codinggame.solo.difficult

object MagicalFrog {

  import scala.annotation.tailrec
  def toBits(n: Int): List[Boolean] = {
    @tailrec
    def loop(x: Int, acc: List[Boolean]): List[Boolean] =
      if (x <= 0) acc
      else loop(x / 2, (x % 2 > 0) :: acc)

    loop(n, Nil)
  }
  trait Monoid[X] {

    def neutral: X

    def add(left: X, right: X): X

    def power(x: X, n: Int): X =
      toBits(n).reverse
        .foldLeft((neutral, x))((acc, b) =>
          (if (b) add(acc._2, acc._1) else acc._1, add(acc._2, acc._2))
        )
        ._1
  }

  case class SquareMatrix(v: List[List[BigInt]]) {
    def mult(that: SquareMatrix): SquareMatrix = {
      val cols = that.v.transpose
      SquareMatrix(
        v.map { row =>
          cols.map { col =>
            row.zip(col).map { case (a, b) => a * b }.sum
          }
        }
      )
    }

    def vect(x: List[BigInt]): List[BigInt] =
      List.range(0, x.size).map(i => List.range(0, x.size).map(k => v(i)(k) * x(k)).sum)
  }

  def squareMonoid(n: Int): Monoid[SquareMatrix] = new Monoid[SquareMatrix] {

    override def neutral: SquareMatrix = SquareMatrix(
      List.range(0, n).map(i => List.range(0, n).map(j => if (i == j) BigInt(1) else BigInt(0)))
    )

    override def add(left: SquareMatrix, right: SquareMatrix): SquareMatrix = left.mult(right)
  }

  def solve(n: Long, k: Int): Int = {
    val map0: Map[Long, BigInt] = init(k.toLong, Map(1L -> BigInt(1)))

    val o =
      if (n <= k) map0(n)
      else {

        val matrix: SquareMatrix = SquareMatrix(
          List
            .range(0, k - 1)
            .map(i => List.range(0, k).map(j => if (j == i + 1) BigInt(1) else BigInt(0))) ++ List(
            List.range(0, k).map(_ => BigInt(1))
          )
        )
        val u0: List[BigInt] = List.range(0, k).map(kk => map0(kk + 1))
        val exp = squareMonoid(k)
          .power(matrix, n.toInt - 1)
        val out = exp.vect(u0)
        out.head
      }

    o.mod(BigInt(1000000000L + 7)).toInt
  }

  def init(k: Long, map: Map[Long, BigInt]): Map[Long, BigInt] = if (map.contains(k)) map
  else {
    List.range(2, k + 1).foldLeft(map) { (acc, j) =>
      val z = List.range(1, j).map(x => acc(x)).sum + BigInt(1)
      acc + (j -> z)
    }
  }

}
