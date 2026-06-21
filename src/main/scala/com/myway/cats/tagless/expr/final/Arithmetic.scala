package com.myway.cats.tagless.expr.`final`


// algebra
trait Arithmetic[Expr] {
  def +(l: Expr, r: Expr): Expr

  def -(l: Expr, r: Expr): Expr

  def *(l: Expr, r: Expr): Expr

  def /(l: Expr, r: Expr): Expr

  def literal(value: Double): Expr
}

// interpreter
object DoubleArithmetic extends Arithmetic[Double] {
  def +(l: Double, r: Double): Double = l + r

  def -(l: Double, r: Double): Double = l - r

  def *(l: Double, r: Double): Double = l * r

  def /(l: Double, r: Double): Double = l / r

  def literal(value: Double): Double = value
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