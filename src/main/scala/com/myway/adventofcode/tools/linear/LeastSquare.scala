package com.myway.adventofcode.tools.linear

object LeastSquare {

  def leastSquaresSolveTr(A: Array[Array[Double]], b: Array[Double]): Array[Double] =
    leastSquaresSolve(
    transpose(A),b)

  /**
   * Solves overdetermined system Ax = b (m ≥ n) using least-squares (normal equations).
   * Minimizes ||Ax - b||₂.
   * Assumes full column rank; throws if AᵀA is singular.
   */
  def leastSquaresSolve(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val m = A.length
    if (m == 0 || A.isEmpty) throw new IllegalArgumentException("Empty matrix")
    val n = A(0).length
    if (b.length != m || A.exists(_.length != n)) {
      throw new IllegalArgumentException("Invalid dimensions")
    }
    if (m < n) {
      throw new IllegalArgumentException("Underdetermined systems (m < n) not supported here")
    }

    // Special case: square → use direct solver (better numerically)
    if (m == n) {
      return solveSquare(A, b)
    }

    val At = transpose(A)
    val AtA = matrixMultiply(At, A)          // n×n
    val Atb = matrixVectorMultiply(At, b)     // length n

    solveSquare(AtA, Atb)
  }
  /**
   * Solves square linear system Ax = b using Gaussian elimination with partial pivoting.
   */
  private def solveSquare(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val n = A.length

    if (b.length != n || A.exists(_.length != n)) {
      throw new IllegalArgumentException("A must be square and b must have length n")
    }

    val aug = Array.ofDim[Double](n, n + 1)
    for (i <- 0 until n) {
      Array.copy(A(i), 0, aug(i), 0, n)
      aug(i)(n) = b(i)
    }

    val eps = 1e-12

    for (k <- 0 until n) {
      var pivotRow = k
      for (i <- k + 1 until n) {
        if (math.abs(aug(i)(k)) > math.abs(aug(pivotRow)(k))) {
          pivotRow = i
        }
      }

      if (math.abs(aug(pivotRow)(k)) < eps) {
        throw new RuntimeException("Matrix is singular or nearly singular")
      }

      if (pivotRow != k) {
        val temp = aug(k)
        aug(k) = aug(pivotRow)
        aug(pivotRow) = temp
      }

      for (i <- k + 1 until n) {
        val factor = aug(i)(k) / aug(k)(k)
        for (j <- k until n + 1) {
          aug(i)(j) -= factor * aug(k)(j)
        }
      }
    }

    val x = new Array[Double](n)
    for (i <- n - 1 to 0 by -1) {
      var sum = aug(i)(n)
      for (j <- i + 1 until n) {
        sum -= aug(i)(j) * x(j)
      }
      x(i) = sum / aug(i)(i)
    }

    x
  }

  // Helper: matrix transpose
  private def transpose(A: Array[Array[Double]]): Array[Array[Double]] = {
    val m = A.length
    if (m == 0) return Array.empty
    val n = A(0).length
    val At = Array.ofDim[Double](n, m)
    for (i <- 0 until m; j <- 0 until n) {
      At(j)(i) = A(i)(j)
    }
    At
  }

  // Helper: matrix-matrix multiply (A is p×q, B is q×r)
  private def matrixMultiply(A: Array[Array[Double]], B: Array[Array[Double]]): Array[Array[Double]] = {
    val p = A.length
    val q = A(0).length
    val r = B(0).length
    val C = Array.ofDim[Double](p, r)
    for (i <- 0 until p; j <- 0 until r) {
      var sum = 0.0
      for (k <- 0 until q) {
        sum += A(i)(k) * B(k)(j)
      }
      C(i)(j) = sum
    }
    C
  }

  // Helper: matrix-vector multiply
  private def matrixVectorMultiply(A: Array[Array[Double]], v: Array[Double]): Array[Double] = {
    val m = A.length
    val n = A(0).length
    val res = new Array[Double](m)
    for (i <- 0 until m) {
      var sum = 0.0
      for (j <- 0 until n) {
        sum += A(i)(j) * v(j)
      }
      res(i) = sum
    }
    res
  }

}
