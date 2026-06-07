package com.myway.codinggame.solo.difficult.variant

object TheResistance {

  trait Monoid[X] {

    def neutral: X

    def add(left: X, right: X): X

  }

  object MonoidSyntax {

    @inline final def apply[A](implicit ev: Monoid[A]): Monoid[A] = ev

    implicit val intPlusMonoid: Monoid[Int] = new Monoid[Int] {
      override def neutral: Int = 0

      override def add(left: Int, right: Int): Int = left + right
    }

    implicit val intMultMonoid: Monoid[Int] = new Monoid[Int] {
      override def neutral: Int = 1

      override def add(left: Int, right: Int): Int = left * right
    }

    implicit val longPlusMonoid: Monoid[Long] = new Monoid[Long] {
      override def neutral: Long = 0L

      override def add(left: Long, right: Long): Long = left + right
    }

    implicit val longMultMonoid: Monoid[Long] = new Monoid[Long] {
      override def neutral: Long = 1L

      override def add(left: Long, right: Long): Long = left * right
    }

    implicit val stringMonoid: Monoid[String] = new Monoid[String] {
      override def neutral: String = ""

      override def add(left: String, right: String): String = left ++ right
    }
  }

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
  }

  object Forest {

    def empty[A]: Forest[A]                     = Forest(Nil)
    def single[A](a: A): (A, Forest[A])         = (a, empty)
    def branch[A](forest: List[(A, Forest[A])]) = Forest(forest)

    def initZipper[A]: ForestZipper[A] = ForestZipper(empty, Nil)
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
  case class Trie[V: Monoid](zipper: ForestZipper[(Char, V)]) {

    def insert(s: String, v: V): Trie[V] = insertChars(s.toList, v).upRoot

    @tailrec
    private def insertChars(l: List[Char], v: V): Trie[V] = l match {
      case Nil =>
        this.zipper.getFocusValue match {
          case None => this
          case Some(old) =>
            val oldC = old._1
            this.copy(zipper = this.zipper.setValue((oldC, implicitly[Monoid[V]].add(old._2, v))))
        }

      case x :: xs =>
        val zp = zipper.downBy(_._1, x) match {
          case None    => zipper.prepend((x, implicitly[Monoid[V]].neutral)).downBy(_._1, x).get
          case Some(z) => z
        }
        Trie(zp).insertChars(xs, v)
    }

    def search(s: String): Option[V] = searchChars(s.toList)

    @tailrec
    private def searchChars(cs: List[Char]): Option[V] = cs match {
      case Nil => zipper.getFocusValue.map(_._2)
      case x :: xs =>
        zipper.downBy(_._1, x) match {
          case None    => None
          case Some(z) => Trie(z).searchChars(xs)
        }
    }

    def upRoot = Trie(zipper.upRoot)

  }

  object Trie {

    def empty[V](implicit M: Monoid[V]) = new Trie[V](Forest.initZipper)
  }
  val morse = Array(
    ".-",
    "-...",
    "-.-.",
    "-..",
    ".",
    "..-.",
    "--.",
    "....",
    "..",
    ".---",
    "-.-",
    ".-..",
    "--",
    "-.",
    "---",
    ".--.",
    "--.-",
    ".-.",
    "...",
    "-",
    "..-",
    "...-",
    ".--",
    "-..-",
    "-.--",
    "--.."
  )

  def idx(c: Char): Int = if (c == '.') 0 else 1

  def split(s: String): List[(String, String)] =
    List.range(1, s.length).map(i => (s.take(i), s.drop(i)))
  def wordToMorse(s: String): String = s.toList.map(c => morse(c - 'A')).mkString
  def solve(l: List[String]): Long = {
    val message: String        = l.head
    val setWords: List[String] = l.tail.tail
    import MonoidSyntax.longPlusMonoid
    val trieMorse = setWords.foldLeft(Trie.empty[Long])((acc, w) => acc.insert(wordToMorse(w), 1L))
    solveShort(message, trieMorse)
  }
  
  private def solveShort(message: String, trieMorse: Trie[Long]): Long = {
    def go(msg: String, out: Map[String, Long]): Map[String, Long] =
      if (msg.isEmpty) Map("" -> 1L)
      else if (out.contains(msg)) out
      else {
        val o: Long = (1 to msg.length).foldLeft(0L) { (acc, i) =>
          trieMorse.search(msg.substring(0, i)) match {
            case Some(cnt) if cnt > 0 =>
              acc + cnt * go(msg.substring(i), out)(msg.substring(i))
            case _ =>
              acc
          }
        }
        out - msg + (msg -> o)
      }

    go(message, Map())(message)
  }

}
