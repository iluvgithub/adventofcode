package com.myway.cats.tagless.expr.`final`


// algebra
trait Arithmetic[Expr] {
  def +(l: Expr, r: Expr): Expr

  def -(l: Expr, r: Expr): Expr

  def *(l: Expr, r: Expr): Expr

  def /(l: Expr, r: Expr): Expr

  def literal(value: Double): Expr
}

// algebra extension
trait Trigonometry[Expr] {
  def sin(expr: Expr): Expr
}

// interpreter
object DoubleArithmetic extends Arithmetic[Double] {
  def +(l: Double, r: Double): Double = l + r

  def -(l: Double, r: Double): Double = l - r

  def *(l: Double, r: Double): Double = l * r

  def /(l: Double, r: Double): Double = l / r

  def literal(value: Double): Double = value
}

object DoubleTrigonometry extends Trigonometry[Double] {

  override def sin(expr: Double): Double = Math.sin(expr)
}


object PrintArithmetic extends Arithmetic[String] {
  def +(l: String, r: String): String =
    s"($l + $r)"

  def -(l: String, r: String): String =
    s"($l - $r)"

  def *(l: String, r: String): String =
    s"($l * $r)"

  def /(l: String, r: String): String =
    s"($l / $r)"

  def literal(value: Double): String =
    value.toString
}

object PrintTrigonometry extends Trigonometry[String] {

  override def sin(expr: String): String = s"sin($expr)"
}


// interpreter extension
object BigDecimalArithmetic extends Arithmetic[BigDecimal] {
  def +(l: BigDecimal, r: BigDecimal): BigDecimal = l + r

  def -(l: BigDecimal, r: BigDecimal): BigDecimal = l - r

  def *(l: BigDecimal, r: BigDecimal): BigDecimal = l * r

  def /(l: BigDecimal, r: BigDecimal): BigDecimal = l / r

  def literal(value: Double): BigDecimal = value
}

object BigDecimalTrigonometry extends Trigonometry[BigDecimal] {

  override def sin(expr: BigDecimal): BigDecimal = BigDecimal(Math.sin(expr.toDouble))
}
