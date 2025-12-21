package com.myway.adventofcode.tools.linear

object LinearSolver {
  /** Solves the system Ax = b using Gaussian elimination with partial pivoting.
   *
   * @param A the coefficient matrix (n x n)
   * @param b the right-hand side vector (length n)
   * @return the solution vector x (length n)
   * @throws IllegalArgumentException if the matrix is singular or inconsistent
   */
  def solve(A: Array[Array[Double]], b: Array[Double]): Array[Double] = { val n = A.length

    // Basic dimension checks
    if (b.length != n || A.exists(_.length != n)) {
      throw new IllegalArgumentException("A must be square and b must have length n")
    }

    // Create augmented matrix [A | b]
    val aug = Array.ofDim[Double](n, n + 1)
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        aug(i)(j) = A(i)(j)
      }
      aug(i)(n) = b(i)
    }

    val eps = 1e-12 // tolerance for detecting near-zero pivots

    // Forward elimination with partial pivoting
    for (k <- 0 until n) {
      // Find pivot row
      var pivotRow = k
      for (i <- k + 1 until n) {
        if (math.abs(aug(i)(k)) > math.abs(aug(pivotRow)(k))) {
          pivotRow = i
        }
      }

      // Check for singularity
      if (math.abs(aug(pivotRow)(k)) < eps) {
        throw new RuntimeException("Matrix is singular or nearly singular")
      }

      // Swap rows if necessary
      if (pivotRow != k) {
        val temp = aug(k)
        aug(k) = aug(pivotRow)
        aug(pivotRow) = temp
      }

      // Eliminate below pivot
      for (i <- k + 1 until n) {
        val factor = aug(i)(k) / aug(k)(k)
        for (j <- k until n + 1) { // can start at k to save a little work
          aug(i)(j) -= factor * aug(k)(j)
        }
      }
    }

    // Back substitution
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
}
