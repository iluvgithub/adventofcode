package com.myway.adventofcode.tools.rosetree

case class RoseTree[A](head: A, kindred: List[RoseTree[A]]) {

  def browseDepth: List[A] = browse(a => b => a ++ b, Nil).reverse
  def browseWidth: List[A] = browse(a => b => b ++ a, Nil).reverse

  import scala.annotation.tailrec
  @tailrec
  private def browse(
    concat: List[RoseTree[A]] => List[RoseTree[A]] => List[RoseTree[A]],
    out: List[A]
  ): List[A] = kindred match {
    case Nil => head :: out
    case x::xs => RoseTree(x.head, concat(x.kindred)(xs)). browse(concat, head::out)
  }


}

object RoseTree {

  def leaf[A](a: A): RoseTree[A] = branch(a, Nil)

  def branch[A](a: A, rs: List[RoseTree[A]]): RoseTree[A] = RoseTree[A](a, rs)
}
