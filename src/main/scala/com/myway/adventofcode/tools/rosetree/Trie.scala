package com.myway.adventofcode.tools.rosetree

import scala.annotation.tailrec

case class Trie(zipper: ForestZipper[Char]) {

  def this() = this(Forest.initZipper)

  def insert(s: String): Trie = insertChars(s.toList).upRoot

  @tailrec
  private def insertChars(l: List[Char]): Trie = l match {
    case Nil => this
    case x :: xs =>
      val zp: ForestZipper[Char] = zipper.downBy(x) match {
        case None    => zipper.prepend(x).downBy(x).get
        case Some(z) => z
      }
      Trie(zp).insertChars(xs)
  }

  def search(s: String): Boolean = searchChars(s.toList)

  @tailrec
  private def searchChars(cs: List[Char]): Boolean = cs match {
    case Nil => true
    case x::xs =>
      zipper.downBy(x) match {
        case None => false
        case Some(z) => Trie(z).searchChars(xs)
      }
  }

  def upRoot =   Trie(zipper.upRoot)

}
