package com.myway.adventofcode.tools.linear

object UndeterminedSolver {
  private val EPS = 1e-12

  def solve(A: Array[Array[Double]], b: Array[Double]): Array[Double] = {
    val m = A.length
    if (m == 0) {
      if (b.isEmpty) return Array.empty[Double]
      else throw new IllegalArgumentException("Empty A but non-empty b")
    }
    if (b.length != m) {
      throw new IllegalArgumentException("b must have same length as number of rows in A")
    }

    // Determine number of variables (columns) safely
    val n = A(0).length
    // Validate all rows have the same number of columns
    var i = 1
    while (i < m) {
      if (A(i).length != n) {
        throw new IllegalArgumentException(s"Row $i has ${A(i).length} columns, expected $n")
      }
      i += 1
    }

    // Create augmented matrix [A | b] with safe copying
    val aug = new Array[Array[Double]](m)
    i = 0
    while (i < m) {
      val row = new Array[Double](n + 1)
      val aRow = A(i)
      var j = 0
      while (j < n) {
        row(j) = aRow(j)
        j += 1
      }
      row(n) = b(i)
      aug(i) = row
      i += 1
    }

    // Gaussian elimination with partial pivoting
    val pivotCol = new Array[Int](m)  // pivotCol[rank] = column index
    var rank = 0
    var col = 0

    while (col < n && rank < m) {
      // Find pivot: row with max abs value in column 'col', from rank downward
      var maxRow = rank
      var maxVal = math.abs(aug(rank)(col))
      var rowIdx = rank + 1
      while (rowIdx < m) {
        val absVal = math.abs(aug(rowIdx)(col))
        if (absVal > maxVal) {
          maxVal = absVal
          maxRow = rowIdx
        }
        rowIdx += 1
      }

      if (maxVal < EPS) {
        // No significant pivot in this column → skip (free variable)
        col += 1
      } else {
        // Swap rows
        if (maxRow != rank) {
          val temp = aug(rank)
          aug(rank) = aug(maxRow)
          aug(maxRow) = temp
        }

        // Forward elimination below pivot
        val pivot = aug(rank)(col)
        rowIdx = rank + 1
        while (rowIdx < m) {
          if (math.abs(aug(rowIdx)(col)) >= EPS) {
            val factor = aug(rowIdx)(col) / pivot
            var j = col
            while (j <= n) {
              aug(rowIdx)(j) -= factor * aug(rank)(j)
              j += 1
            }
          }
          rowIdx += 1
        }

        pivotCol(rank) = col
        rank += 1
        col += 1
      }
    }

    // Check for inconsistency: non-zero b in zero rows
    var row = rank
    while (row < m) {
      if (math.abs(aug(row)(n)) > EPS) {
        throw new RuntimeException("Inconsistent system: no solution exists")
      }
      row += 1
    }

    // Back-substitution
    val x = new Array[Double](n)

    // Process pivot rows from bottom to top
    var r = rank - 1
    while (r >= 0) {
      val pc = pivotCol(r)
      var sum = aug(r)(n)
      var j = pc + 1
      while (j < n) {
        sum -= aug(r)(j) * x(j)
        j += 1
      }
      x(pc) = sum / aug(r)(pc)
      r -= 1
    }
    // Free variables remain 0.0 (already initialized)

    x
  }
}
