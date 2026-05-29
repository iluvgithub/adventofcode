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
    case Nil     => head :: out
    case x :: xs => RoseTree(x.head, concat(x.kindred)(xs)).browse(concat, head :: out)
  }

  def trace: String = fold[String](a =>
    zs =>
      if (zs.isEmpty) a.toString else s"${a.toString}(${if (zs.isEmpty) "" else zs.mkString(",")})"
  )
  def fold[B](f: A => List[B] => B): B = {

    sealed trait Frame
    case class Enter(tree: RoseTree[A])                                     extends Frame
    case class Exit(a: A, cont: List[B] => B, remaining: Int, acc: List[B]) extends Frame

    @tailrec
    def loop(stack: List[Frame], resultStack: List[B]): B = stack match {
      case Nil =>
        resultStack.head

      case Enter(RoseTree(a, subs)) :: tail =>
        val exit     = Exit(a, f(a), subs.length, Nil)
        val newStack = subs.map(Enter).reverse ++ (exit :: tail)
        loop(newStack, resultStack)

      case Exit(a, cont, 0, acc) :: tail =>
        val b = cont(acc.reverse)
        loop(tail, b :: resultStack)

      case Exit(a, cont, n, acc) :: tail =>
        resultStack match {
          case h :: t =>
            loop(Exit(a, cont, n - 1, h :: acc) :: tail, t)
          case Nil =>
            throw new IllegalStateException("Result stack underflow")
        }
    }

    loop(List(Enter(this)), Nil)
  }
}

object RoseTree {

  def leaf[A](a: A): RoseTree[A] = branch(a, Nil)

  def branch[A](a: A, rs: List[RoseTree[A]]): RoseTree[A] = RoseTree[A](a, rs)

}
