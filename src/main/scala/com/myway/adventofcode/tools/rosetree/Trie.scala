package com.myway.adventofcode.tools.rosetree

import com.myway.adventofcode.tools.monoid.Monoid

import scala.annotation.tailrec

case class Trie[V: Monoid](zipper: ForestZipper[(Char, V)]) {

  def insert(s: String, v: V): Trie[V] = insertChars(s.toList, v).upRoot

  @tailrec
  private def insertChars(l: List[Char], v: V): Trie[V] = l match {
    case Nil =>
      this.zipper.getFocusValue match {
        case None => this
        case Some(old) =>
          val oldC = old._1
          this.copy(zipper=
            this.zipper.setValue((oldC, implicitly[Monoid[V]].add(old._2, v)))
          )
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

  def empty[V](implicit M: Monoid[V]) = new Trie[V](Forest.initZipper )
}
