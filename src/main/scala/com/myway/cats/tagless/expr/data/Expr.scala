package com.myway.cats.tagless.expr.data

trait Expr {

}

case class Add(l: Expr, r: Expr) extends Expr

case class Sub(l: Expr, r: Expr) extends Expr

case class Mul(l: Expr, r: Expr) extends Expr

case class Div(l: Expr, r: Expr) extends Expr

case class Literal(value: Double) extends Expr