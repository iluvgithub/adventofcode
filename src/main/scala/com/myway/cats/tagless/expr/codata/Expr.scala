package com.myway.cats.tagless.expr.codata

trait Expr {
  self =>
  def eval: Double

  def print: String

  def add(that: Expr): Expr = new Expr {
    def eval: Double = self.eval + that.eval

    def print: String = s"(${self.print} + ${that.print})"
  }

  def sub(that: Expr): Expr = new Expr {
    def eval: Double = self.eval - that.eval

    def print: String = s"(${self.print} - ${that.print})"
  }

  def mult(that: Expr): Expr = new Expr {
    def eval: Double = self.eval * that.eval

    def print: String = s"(${self.print} * ${that.print})"
  }

  def div(that: Expr): Expr = new Expr {
    def eval: Double = self.eval / that.eval

    def print: String = s"(${self.print} / ${that.print})"
  }


}

object Expr {

  def literal(value: Double): Expr =
    new Expr {
      def eval: Double = value

      def print: String = value.toString
    }

  val ZERO: Expr = literal(0)

  def sin(expr: Expr):Expr = new Expr {
    override def eval: Double = Math.sin(expr.eval)

    override def print: String = s"sin(${expr.print})"
  }

}