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
        go(
          phi,
          f.roses.map(_._2).map(ForestWrap(_)).reverse ++ (BranchStub(heads, heads.size) :: vs),
          out
        )

      case BranchStub(as, n) :: vs =>
        val children  = as.zip(out.take(n))
        val remaining = out.drop(n)
        go(phi, vs, phi(children) :: remaining)
    }

    go(f, ForestWrap(this) :: Nil, Nil)
  }

  def trace: String =
    fold[String](xs =>
      s"${xs
          .map { case (u, v) =>
            if (v.isEmpty) s"$u"
            else
              s"$u($v)"
          }
          .mkString(",")}"
    )

  def browseDepth: List[A] = fold[List[A]](_.flatMap { case (a, as) => a :: as })
}

object Forest {

  def empty[A]: Forest[A]                     = Forest(Nil)
  def single[A](a: A): (A, Forest[A])         = (a, empty)
  def branch[A](forest: List[(A, Forest[A])]) = Forest(forest)

  def initZipper[A]: ForestZipper[A] = ForestZipper(empty, Nil)


  def insertInto(f:Forest[Char], s:String):Forest[Char] =
    insert(new ForestZipper[Char](f,Nil), s.toList)

  @tailrec
  def insert(f0: ForestZipper[Char], chrs: List[Char]): Forest[Char] = chrs match {
    case Nil => f0.upRoot.focus
    case x :: xs =>
      val zz = f0.downBy(x) match {
        case None    => f0.prepend(x).downBy(x).get
        case Some(z) => z
      }
      insert(zz, xs)
  }
}

case class ForestContext[A](
  lefts: List[(A, Forest[A])], // siblings before parent
  parent: A,                   // parent label
  rights: List[(A, Forest[A])] // siblings after parent
)
case class ForestZipper[A](focus: Forest[A], context: List[ForestContext[A]]) {

  def down(i: Int): Option[ForestZipper[A]] = if (i >= focus.roses.size) None
  else {
    val left               = focus.roses.take(i)
    val af: (A, Forest[A]) = focus.roses.drop(i).head
    val right              = focus.roses.drop(i + 1)

    Some(
      ForestZipper(af._2, ForestContext(left, af._1, right) :: context)
    )
  }

  def search[K](get: A => K, k: K): Option[Int] =
    List.range(0, focus.roses.length).find(i => get(focus.roses(i)._1) == k)

  def search(a: A): Option[Int] = search(identity, a)

  def downBy(a: A): Option[ForestZipper[A]] = downBy(identity, a)
  def downBy[K](get: A => K, k: K): Option[ForestZipper[A]] = for {
    i <- search(get, k)
    z <- down(i)
  } yield z

  def up: Option[ForestZipper[A]] = context match {
    case Nil => None
    case x :: xs =>
      Some(ForestZipper(Forest(x.lefts ++ ((x.parent, focus) :: x.rights)), xs))

  }

  @tailrec
  final def upRoot: ForestZipper[A] = up match {
    case None    => this
    case Some(z) => z.upRoot
  }

  def prepend(a: A): ForestZipper[A] =
    this.copy(
      focus = Forest((a, Forest.empty[A]) :: focus.roses)
    )

  def setValue(a: A): ForestZipper[A] = this.context match {
    case Nil     => this
    case x :: xs => this.copy(context = x.copy(parent = a) :: xs)
  }

  def getFocusValue: Option[A] = this.context match {
    case Nil    => None
    case x :: _ => Some(x.parent)
  }
}
