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

  def trace: String = fold[String](a => xs => if (xs.isEmpty) s"$a" else s"$a(${xs.mkString(",")})")

  def fold[B](f: A => List[B] => B): B = {
    trait ToVisit

    case class BranchStub(lbl: A, n: Int) extends ToVisit
    case class TreeWrap(t: RoseTree[A])   extends ToVisit

    @annotation.tailrec
    def loop(stack: List[ToVisit], results: List[B]): B = stack match {
      case Nil => results.head

      case TreeWrap(t) :: rest =>
        val newStack =
          t.kindred.map(TreeWrap(_)) ++ (BranchStub(t.head, t.kindred.length) :: rest)
        loop(newStack, results)

      case BranchStub(lbl, n) :: rest =>
        val children  = results.take(n)
        val remaining = results.drop(n)
        val value     = f(lbl)(children.reverse)
        loop(rest, value :: remaining)
    }

    loop(List(TreeWrap(this)), Nil)
  }
}

object RoseTree {

  def leaf[A](a: A): RoseTree[A] = branch(a, Nil)

  def branch[A](a: A, rs: List[RoseTree[A]]): RoseTree[A] = RoseTree[A](a, rs)

}
