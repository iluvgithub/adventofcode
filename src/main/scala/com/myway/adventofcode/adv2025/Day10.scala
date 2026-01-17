package com.myway.adventofcode.adv2025

import com.myway.adventofcode.tools.astar.{Astar, AstarGraph}
import com.myway.adventofcode.tools.file.FileUtil
import com.myway.adventofcode.tools.list.ListUtil
import com.myway.adventofcode.tools.parser.ParserErrorMonad._

object Day10 {

  def main(args: Array[String]): Unit = {
    val data: List[String] = FileUtil.readFile("adventofcode/2025/day10.txt")
    println(solve1(data))
    println(
      solve2(data)
    ) // 16855 too low, 17209 too low 17842 too low, 17846 not good, 17850 ??? 17856/
  }

  def solve1(data: List[String]): Long = {
    val machines: List[Machine] = data.map(machineParser.parseSure)
    machines.map(_.solve).sum
  }

  def solve2(data: List[String]): Long = {
    val machines: List[Machine] = data.map(machineParser.parseSure)
    machines.map(_.solve2).sum
  }

  case class Machine(light: List[Boolean], buttons: List[Button], joltage: List[Int]) {

    def solve: Long = {

      val g = new AstarGraph[List[Boolean]] {
        override def next(bs: List[Boolean]): List[List[Boolean]] = buttons.map(_.press(bs))
      }
      val cost: List[Boolean] => List[Boolean] => Long = x => y => 1L
      val out = Astar.find(g, cost, light.map(_ => false), light)
      out.size - 1
    }

    def solve2: Long = {
      val target                  = joltage
      val sz                      = buttons.map(_.ids.max).max + 1
      val matrix: List[List[Int]] = buttons.map(new Joltage(_, sz).joltage).transpose

      val o: (List[List[Double]], List[Double]) = gjr(matrix, target)
      val a1: List[List[Double]]                = o._1
      val b1: List[Double]                      = o._2

      TriangularSolver.solve(matrix, joltage, a1, b1).sum

    }

    def solve2b: Long = {
      val target: Joltage = Joltage(joltage)
      val sz              = buttons.map(_.ids.max).max + 1
      val empty: Joltage  = Joltage(List.range(0, sz).map(_ => 0))
      val map: Map[Joltage, Int] =
        buttons.map(new Joltage(_, sz)).map(j => j -> 1).toMap ++ Map(empty -> 0)
      loop(map, target)
    }

    def loop(map: Map[Joltage, Int], target: Joltage): Long = if (map.contains(target))
      map(target).toLong
    else {
      val taggedKeys: List[(Joltage, Int)] = map.keySet.toList.zipWithIndex
      val pairs: List[(Joltage, Joltage)] = for {
        ki <- taggedKeys
        kj <- taggedKeys
        if ki._2 < kj._2
      } yield (ki._1, kj._1)
      val newMap: Map[Joltage, Int] = pairs.foldLeft(map) { (acc, pair) =>
        val k1: Joltage  = pair._1
        val k2: Joltage  = pair._2
        val key: Joltage = k1.add(k2)
        if (key.beyond(target)) {
          val v = Math.min(map(k1) + map(k2), acc.getOrElse(key, Integer.MAX_VALUE))
          acc - key + (key -> v)
        } else acc
      }

      // println(s"tgt=$target/sz=${newMap.size},$newMap")
      loop(newMap, target)
    }

  }

  case class Joltage(joltage: List[Int]) {

    def add(that: Joltage): Joltage = Joltage(
      this.joltage.zip(that.joltage).map { case (u, v) => u + v }
    )

    def beyond(that: Joltage): Boolean = joltage.zip(that.joltage).forall { case (u, v) => u <= v }

    def this(from: Button, sz: Int) = this(from.toArray(sz).toList)
  }

  def machineParser: ParserError[Machine] = for {
    _       <- char('[')
    light   <- many(orElse(char('.').map(_ => false), char('#').map(_ => true)))
    _       <- char(']')
    _       <- char(' ')
    buttons <- many(token(subParser))
    _       <- token(char('{'))
    joltage <- token(many(tokenOf(',', digits)))
    _       <- char('}')

  } yield Machine(light, buttons, joltage.map(_.toInt))

  case class Button(ids: List[Int]) {
    def press(bs: List[Boolean]): List[Boolean] =
      bs.zipWithIndex.map(x => if (ids.toSet.contains(x._2)) !x._1 else x._1)

    def toArray(sz: Int): Array[Int] = List
      .range(0, sz)
      .map(i => if (ids.contains(i)) 1 else 0)
      .toArray

  }

  val subParser: ParserError[Button] = for {
    _   <- char('(')
    ids <- many(tokenOf(',', digits))
    _   <- char(')')
  } yield Button(ids.map(_.toInt))

  import scala.math.abs

  def gjr(a: List[List[Int]], b: List[Int]): (List[List[Double]], List[Double]) = {
    val m = a.length
    if (m == 0) return (Nil, Nil)
    val n = a.head.length
    if (b.length != m || a.exists(_.length != n)) {
      throw new IllegalArgumentException("Invalid dimensions")
    }

    // Create mutable augmented matrix [A | b] with Double entries
    val mat = Array.ofDim[Double](m, n + 1)
    for (i <- 0 until m) {
      for (j <- 0 until n)
        mat(i)(j) = a(i)(j).toDouble
      mat(i)(n) = b(i).toDouble
    }

    // Gauss-Jordan elimination to reduced row echelon form (RREF)
    var row = 0 // current pivot row
    var col = 0 // current column
    while (row < m && col < n) {
      // Partial pivoting: find row with largest absolute value in current column
      var pivotRow = row
      for (i <- row + 1 until m)
        if (abs(mat(i)(col)) > abs(mat(pivotRow)(col))) {
          pivotRow = i
        }

      // If pivot is (approximately) zero, skip this column
      if (abs(mat(pivotRow)(col)) < 1e-12) {
        col += 1
      } else {
        // Swap rows to bring pivot to current position
        if (pivotRow != row) {
          val temp = mat(row)
          mat(row) = mat(pivotRow)
          mat(pivotRow) = temp
        }

        // Scale pivot row to make pivot = 1 (divide entire row)
        val pivot = mat(row)(col)
        for (j <- 0 until n + 1)
          mat(row)(j) /= pivot

        // Eliminate column entries above and below pivot (all other rows)
        for (i <- 0 until m if i != row) {
          val factor = mat(i)(col)
          for (j <- 0 until n + 1)
            mat(i)(j) -= factor * mat(row)(j)
        }

        row += 1
        col += 1
      }
    }

    // Extract transformed matrix A1 and vector b1
    val a1 = (for (i <- 0 until m) yield (for (j <- 0 until n) yield mat(i)(j)).toList).toList

    val b1 = (for (i <- 0 until m) yield mat(i)(n)).toList

    (a1, b1)
  }

  def solveUpperTriangular(t: List[List[Double]], b: List[Double]): List[Double] = {
    val n = t.length
    if (n == 0) return Nil
    if (b.length != n || t.exists(_.length != n)) {
      throw new IllegalArgumentException(
        s"Matrix must be square and b must match dimension, n=$n, b:${b.length}"
      )
    }

    val x = Array.ofDim[Double](n)

    // Back-substitution using while loops (avoids downto syntax)
    var i = n - 1
    while (i >= 0) {
      var sum = b(i)
      var j   = i + 1
      while (j < n) {
        sum -= t(i)(j) * x(j)
        j += 1
      }

      val diag = t(i)(i)
      if (math.abs(diag) < 1e-12) {
        throw new ArithmeticException(
          s"Zero or near-zero on diagonal at position ($i,$i): cannot solve"
        )
      }

      x(i) = sum / diag
      i -= 1
    }

    x.toList
  }

}

object TriangularSolver {

  /** Solves the linear system T * x = b where T is an upper triangular matrix (i > j implies
    * T(i)(j) == 0, 0-based indexing) with positive diagonal entries.
    *
    * This version uses immutable Lists in the public signature for input/output, but internally
    * converts to Arrays for efficient random access during solving.
    *
    * @param t
    *   Upper triangular matrix as List[List[Double]] (n x n)
    * @param b
    *   Right-hand side vector as List[Double] (length n)
    * @return
    *   Solution vector x as List[Double]
    * @throws IllegalArgumentException
    *   if dimensions don't match
    */
  def solveUpperTriangular(t: List[List[Double]], b: List[Double]): List[Double] = {
    if (t.isEmpty) {
      if (b.isEmpty) Nil
      else throw new IllegalArgumentException("Dimension mismatch: empty matrix but non-empty b")
    }

    val n = t.size
    if (b.size != n) {
      throw new IllegalArgumentException(
        s"Dimension mismatch: matrix is ${n}x$n, vector length ${b.size}"
      )
    }
    if (!t.forall(_.size == n)) {
      throw new IllegalArgumentException("Matrix must be square (all rows same length)")
    }

    // Convert to Arrays for O(1) access during back-substitution
    val tArr: Array[Array[Double]] = t.map(_.toArray).toArray
    val bArr: Array[Double]        = b.toArray

    val xArr = new Array[Double](n)

    // Back substitution
    var i = n - 1
    while (i >= 0) {
      // Problem guarantees t(i)(i) > 0, but we keep a safety check
      if (tArr(i)(i) == 0.0) {
        throw new IllegalArgumentException(s"Diagonal entry t($i)($i) is zero")
      }

      var sum = bArr(i)
      var j   = i + 1
      while (j < n) {
        sum -= tArr(i)(j) * xArr(j)
        j += 1
      }
      xArr(i) = sum / tArr(i)(i)
      i -= 1
    }

    xArr.toList
  }

  def solve(
    a: List[List[Int]],
    b0: List[Int],
    t: List[List[Double]],
    b: List[Double]
  ): List[Int] = {
    val nbCols = t(1).size
    val idx: List[Int] = List
      .range(0, nbCols)
      .foldLeft[(Int, List[Int])]((0, Nil))((acc, j) =>
        if (acc._1 >= t.length) acc
        else if (Math.abs(t(acc._1)(j)) > 0) (acc._1 + 1, j :: acc._2)
        else acc
      )
      ._2
      .reverse
    val tri: List[List[Double]] = idx.zipWithIndex.foldLeft[List[List[Double]]](Nil) { (mx, x) =>
      val j                 = x._1
      val i                 = x._2
      val row: List[Double] = t(i).zipWithIndex.filter { case (_, k) => idx.contains(k) }.map(_._1)
      val matrix: List[List[Double]] = mx ++ List(row)
      matrix
    }

    // tri.zipWithIndex.foreach({ case (r, i) => println(s"t[$i] $r") })
    val diff = List.range(0, nbCols).filterNot(idx.contains)

    def computeMax(j: Int): Int =
      List.range(0, t.length).filter(i => a(i)(j) > 0).map(j => b0(j)).min

    val o: List[List[(Int, Long)]] =
      diff.map(x => ListUtil.cpl(x, List.range(0, computeMax(x) + 1).map(_.toLong)))
    val cpList: List[List[(Int, Long)]] = ListUtil.cpList(o)
    val oo = cpList
      .map { l =>
        val b1 = b.take(tri.length).zipWithIndex.map { case (x, i) =>
          l.foldLeft(x)((acc, t2) => acc - t2._2 * t(i)(t2._1))
        }

        val solution: List[Double] = solveUpperTriangular(tri, b1)
        val complete =
          List.range(0, nbCols).foldLeft[(List[Long], Int, Int, Double)]((Nil, 0, 0, 0.0)) {
            (acc, j) =>
              if (idx.contains(j)) {
                val z = solution(acc._2)
                val y = Math.round(z)
                (y :: acc._1, acc._2 + 1, acc._3, Math.max(acc._4, Math.abs(z - y)))
              } else
                (l(acc._3)._2 :: acc._1, acc._2, acc._3 + 1, acc._4)
          }

        if (complete._4 > 0.0000001) Nil
        else
          complete._1.reverse
      }
      .filter(_.forall(_ >= 0))
      .filter(x => mult(a, x.map(_.toInt), b0).equals(0))
      .minBy(_.sum)

    val x: List[Int] = oo.map(_.toInt)

    x
  }

  def mult(a: List[List[Int]], x: List[Int], b: List[Int]): Int = {
    val b1 = List
      .range(0, a.length)
      .map(i => List.range(0, x.size).foldLeft(0)((acc, j) => acc + a(i)(j) * x(j)) - b(i))
    b1.sum
  }

}
