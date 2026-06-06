package com.myway.adventofcode.tools.rosetree

import scala.annotation.tailrec

case class Forest[A](roses: List[(A, Forest[A])]) {

  def fold[B](f: List[(A, B)] => B): B = {
    sealed trait ToVisit
    case class BranchStub(as: List[A], n: Int) extends ToVisit
    case class ForestWrap(f: Forest[A])        extends ToVisit

    @tailrec
    def go(phi: List[(A, B)] => B, visit: List[ToVisit], out: List[B]): B = visit match {
      case Nil => out.head
      case ForestWrap(f) :: vs =>
        val heads: List[A] = f.roses.map(_._1)
        go(phi, f.roses.map(_._2).map(ForestWrap(_)).reverse ++ (BranchStub(heads, heads.size) :: vs), out)

      case BranchStub(as, n) :: vs =>
        val children  = as.zip(out.take(n))
        val remaining = out.drop(n)
        go(phi, vs, phi(children) :: remaining)
    }

    go(f, ForestWrap(this) :: Nil, Nil)
  }

  def trace: String =
    fold[String]( xs =>  s"${xs.map({case(u,v) =>
      if(v.isEmpty) s"$u"
        else
      s"$u($v)"}).mkString(",")}"
    )
}

object Forest {

  def empty[A]: Forest[A]                     = Forest(Nil)
  def single[A](a: A): (A, Forest[A])         = (a, empty)
  def branch[A](forest: List[(A, Forest[A])]) = Forest(forest)
}
