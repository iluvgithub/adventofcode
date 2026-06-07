package com.myway.adventofcode.tools.rosetree

case class BinTree[A](left: Option[BinTree[A]], a: A, right: Option[BinTree[A]]) {

  def fold[B](f: Option[B] => A => Option[B] => B): B = {

    sealed trait Frame
    case class BranchStub(a: A, hasLeft: Boolean, hasRight: Boolean) extends Frame
    case class Wrap(node: BinTree[A])                                extends Frame

    @annotation.tailrec
    def loop(stack: List[Frame], results: List[B]): B =
      stack match {
        case Nil => results.head
        case Wrap(nd) :: rest =>
          val (wraps, stub) =
            if (nd.left.isDefined && nd.right.isDefined)
              (Wrap(nd.left.get) :: Wrap(nd.right.get) :: Nil, BranchStub(nd.a, hasLeft = true, hasRight = true))
            else if (nd.left.isDefined)
              (Wrap(nd.left.get) :: Nil, BranchStub(nd.a, hasLeft = true, hasRight = false))
            else if (nd.right.isDefined)
              (Wrap(nd.right.get) :: Nil, BranchStub(nd.a, hasLeft = false, hasRight = true))
            else (Nil, BranchStub(nd.a, hasLeft = false, hasRight = false))

          val newStack = wraps ++ (stub :: rest)
          loop(newStack, results)

        case BranchStub(a, hasLeft, hasRight) :: rest =>
          val (result, dropSize) =
            if (hasLeft && hasRight) (f(Some(results.tail.head))(a)(Some(results.head)), 2)
            else if (hasLeft) (f(Some(results.head))(a)(None), 1)
            else if (hasRight) (f(None)(a)(Some(results.head)), 1)
            else (f(None)(a)(None), 0)

          loop(rest, result :: results.drop(dropSize))

      }

    loop(List(Wrap(this)), Nil)
  }

  def trace: String = fold[String](ol =>
    s =>
      or =>
        if (ol.isEmpty && or.isEmpty) s.toString
        else s"$s[${ol.getOrElse("")},${or.getOrElse("")}]"
  )
}

object BinTree {
  def leaf[A](a: A): BinTree[A] = BinTree(None, a, None)

}
case class BinTreeContext[A](e: Either[(A, Option[BinTree[A]]), (A, Option[BinTree[A]])])
case class BinTreeZipper[A](focus: BinTree[A], contexts: List[BinTreeContext[A]]) {

  def this(f: BinTree[A]) = this(f, Nil)
  def left: Option[BinTreeZipper[A]] = focus.left match {
    case None => None
    case Some(l) =>
      Some(
        BinTreeZipper(l, BinTreeContext(Right((focus.a, focus.right))) :: contexts)
      )
  }

  def right: Option[BinTreeZipper[A]] = focus.right match {
    case None => None
    case Some(l) =>
      Some(
        BinTreeZipper(l, BinTreeContext(Left((focus.a, focus.left))) :: contexts)
      )
  }

  def up: Option[BinTreeZipper[A]] = contexts match {
    case Nil => None
    case c :: cs =>
      Some(
        BinTreeZipper(
          c.e.fold(
            pair => BinTree(pair._2, pair._1, Some(focus)),
            pair => BinTree(Some(focus), pair._1, pair._2)
          ),
          cs
        )
      )
  }

  def upRoot: BinTreeZipper[A] = up match {
    case None    => this
    case Some(z) => z.upRoot
  }

  def setOnFocus(newA: A): BinTreeZipper[A] = this.copy(focus = focus.copy(a = newA))
  def extendLeft(bin: BinTree[A]): BinTreeZipper[A] =
    this.copy(focus = focus.copy(left = Some(bin)))
  def extendRight(bin: BinTree[A]): BinTreeZipper[A] =
    this.copy(focus = focus.copy(right = Some(bin)))

}
