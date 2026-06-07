package com.myway.codinggame.solo.difficult.variant

object TheResistance {

  import scala.annotation.tailrec
  case class BinTrie(left: Option[BinTrie], value: Long, right: Option[BinTrie]) {
    def this(v: Long) = this(None, v, None)
  }
  def leaf(l: Long): BinTrie = BinTrie(None, l, None)

  case class BinTreeContext(e: Either[(Long, Option[BinTrie]), (Long, Option[BinTrie])])
  case class BinTreeZipper(focus: BinTrie, contexts: List[BinTreeContext]) {

    def this(f: BinTrie) = this(f, Nil)
    def left: Option[BinTreeZipper] = focus.left match {
      case None => None
      case Some(l) =>
        Some(
          BinTreeZipper(l, BinTreeContext(Right((focus.value, focus.right))) :: contexts)
        )
    }

    def right: Option[BinTreeZipper] = focus.right match {
      case None => None
      case Some(l) =>
        Some(
          BinTreeZipper(l, BinTreeContext(Left((focus.value, focus.left))) :: contexts)
        )
    }

    def up: Option[BinTreeZipper] = contexts match {
      case Nil => None
      case c :: cs =>
        Some(
          BinTreeZipper(
            c.e.fold(
              pair => BinTrie(pair._2, pair._1, Some(focus)),
              pair => BinTrie(Some(focus), pair._1, pair._2)
            ),
            cs
          )
        )
    }

    def upRoot: BinTreeZipper = up match {
      case None    => this
      case Some(z) => z.upRoot
    }

    def setOnFocus(newA: Long): BinTreeZipper = this.copy(focus = focus.copy(value = newA))
    def extendLeft(bin: BinTrie): BinTreeZipper =
      this.copy(focus = focus.copy(left = Some(bin)))
    def extendRight(bin: BinTrie): BinTreeZipper =
      this.copy(focus = focus.copy(right = Some(bin)))

  }

  case class MyTrie(zipper: BinTreeZipper) {

    def insert(w: String, v: Long): MyTrie = insertChars(w.toList, v).upRoot
    @tailrec
    private def insertChars(l: List[Char], v: Long): MyTrie = l match {
      case Nil =>
        this.copy(zipper = this.zipper.setOnFocus(this.zipper.focus.value + v))

      case x :: xs =>
        val zp = (if (x == '.') zipper.left else zipper.right) match {
          case None =>
            if (x == '.') zipper.extendLeft(leaf(0L)).left.get
            else zipper.extendRight(leaf(0L)).right.get

          case Some(z) => z
        }
        MyTrie(zp).insertChars(xs, v)
    }

    def search(s: String): Option[Long] = searchChars(s.toList)

    @tailrec
    private def searchChars(cs: List[Char]): Option[Long] = cs match {
      case Nil => Some(zipper.focus.value)
      case x :: xs =>
        (if (x == '.') zipper.left else zipper.right) match {
          case None    => None
          case Some(z) => MyTrie(z).searchChars(xs)
        }
    }

    def upRoot: MyTrie = MyTrie(zipper.upRoot)

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


  def wordToMorse(s: String): String = s.toList.map(c => morse(c - 'A')).mkString
  def solve(input: List[String]): Long = {
    val message = input.head
    val words   = input.drop(2)

    val trie = List
      .range(0, words.length)
      .foldLeft(
        MyTrie(
          new BinTreeZipper(new BinTrie(0L))
        )
      ) { (tr, i) =>
        tr.insert(wordToMorse(words(i)), 1L)
      }

    countWays(message, trie)
  }

  def countWays(message: String, myTrie0: MyTrie): Long = {
    val n    = message.length
    val memo = Array.fill[Long](n + 1)(-1L)
    def dfs(pos: Int): Long = {
      if (pos == n) return 1L
      if (memo(pos) != -1L) return memo(pos)

      var res                           = 0L
      var optZip: Option[BinTreeZipper] = Some(myTrie0.zipper)
      var i                             = pos

      while (i < n && optZip.isDefined) {
        val optCurrentNode =
          if (message.charAt(i) == '.') optZip.flatMap(_.left)
          else optZip.flatMap(_.right)

        if (optCurrentNode.isDefined && optCurrentNode.get.focus.value > 0L) {
          res += optCurrentNode.get.focus.value * dfs(i + 1)
        }
        optZip = optCurrentNode
        i += 1
      }

      memo(pos) = res
      res
    }

    dfs(0)
  }
}
