package com.myway.adventofcode.tools.rosetree

import scala.annotation.tailrec


case class RoseContext[A](lefts: List[RoseTree[A]], a: A, rights: List[RoseTree[A]])

case class RoseZipper[A](focus: RoseTree[A], contexts: List[RoseContext[A]]) {

  def this(rs: RoseTree[A]) = this(rs, Nil)

  def down(i: Int): Option[RoseZipper[A]] =
    if (focus.kindred.size <= i) None
    else Some(
      RoseZipper(
        focus.kindred(i),
        RoseContext(
          focus.kindred.take(i),
          focus.head,
          focus.kindred.drop(i + 1)
        ) :: contexts)
    )

  import scala.annotation.tailrec

  @tailrec
  final def upRoot: RoseZipper[A] = up match {
    case None => this
    case Some(z) => z.upRoot
  }

  def upRootPath: List[Int] = upRootPath(Nil)

  @tailrec
  private def upRootPath(acc: List[Int]): List[Int] = up match {
    case None => acc
    case Some(z) => z.upRootPath(contexts.head.lefts.size :: acc)
  }

  def up: Option[RoseZipper[A]] = contexts match {
    case Nil => None
    case y :: ys => Some(
      RoseZipper(
        RoseTree(y.a, y.lefts ++ (focus :: y.rights)),
        ys
      )
    )
  }

  def append(r: RoseTree[A]): RoseZipper[A] = this.copy(
    focus = RoseTree(this.focus.head, this.focus.kindred ++ List(r))
  )

  def modifyFocusHead(a: A): RoseZipper[A] = copy(
    focus = this.focus.copy(head = a)
  )

  def find(a: A): Option[RoseZipper[A]] = RoseZipperHelp.findRec(a, this :: Nil)
}

object RoseZipperHelp {
  @tailrec
  def findRec[A](a: A, rs: List[RoseZipper[A]]): Option[RoseZipper[A]] = rs match {
    case Nil => None
    case x :: xs =>
      if (x.focus.head == a) Some(x)
      else {
        val n = x.focus.kindred.size
        val newRz: List[RoseZipper[A]] = List.range(0, n).map(x.down(_)).collect(_.get)
        findRec(a, newRz ++ xs)
      }
  }

}