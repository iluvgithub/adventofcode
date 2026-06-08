package com.myway.codinggame.solo.difficult

object TheResistance {

  import scala.annotation.tailrec
  case class BinTrie(left: Option[BinTrie], value: Long, right: Option[BinTrie]) {
    def this(v: Long) = this(None, v, None)

    def fold[B](f: Option[B] => Long => Option[B] => B): B =
      f(left.map(_.fold(f)))(value)(right.map(_.fold(f)))

    def trace: String = fold[String](op1 =>
      l =>
        op2 =>
          if (op1.isEmpty && op2.isEmpty) s"$l"
          else s"$l(${op1.getOrElse("")},${op2.getOrElse("")})"
    )

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

    def depthToUp: Int = depthToUp(0)
    def depthToUp(o: Int): Int = up match {
      case None    => o
      case Some(z) => z.depthToUp(o + 1)
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

  val morseMap: Map[Char, String] = Map(
    'A' -> ".-",
    'B' -> "-...",
    'C' -> "-.-.",
    'D' -> "-..",
    'E' -> ".",
    'F' -> "..-.",
    'G' -> "--.",
    'H' -> "....",
    'I' -> "..",
    'J' -> ".---",
    'K' -> "-.-",
    'L' -> ".-..",
    'M' -> "--",
    'N' -> "-.",
    'O' -> "---",
    'P' -> ".--.",
    'Q' -> "--.-",
    'R' -> ".-.",
    'S' -> "...",
    'T' -> "-",
    'U' -> "..-",
    'V' -> "...-",
    'W' -> ".--",
    'X' -> "-..-",
    'Y' -> "-.--",
    'Z' -> "--.."
  )

  private def wordToMorse(s: String): String = s.toList.map(c => morseMap(c)).mkString

  def solve(l: List[String]): Long = {
    val message: String        = l.head
    val setWords: List[String] = l.tail.tail
    val setWordsMorse: Map[String, Int] =
      setWords.groupBy(wordToMorse).toList.map { case (k, v) => (k, v.size) }.toMap
    solve(message, setWordsMorse)
  }

  case class Step(zippers: List[BinTreeZipper], score: Map[Int, Long])
  private def solve(message: String, setWordsMorse: Map[String, Int]): Long = {

    val wordsTrie: MyTrie =
      setWordsMorse.keySet.foldLeft(MyTrie(new BinTreeZipper(new BinTrie(0L))))((tr, w) =>
        tr.insert(w, setWordsMorse(w))
      )

    val out: Step = List
      .range(0, message.length)
      .foldLeft(Step(wordsTrie.zipper :: Nil, Map(0 -> 1L))) { (step, i) =>
        val c = message.charAt(i)
        val method: List[(BinTreeZipper, Long)] =
          step.zippers
            .map(z => if (c == '.') z.left else z.right)
            .filter(_.isDefined)
            .map(_.get)
            .map(z => {
              val k = i-(z.depthToUp - 1)
              val l = z.focus.value * step.score(k)
              (z, l)
            })
        val sum                         = method.map(_._2).sum
        val remain: List[BinTreeZipper] = wordsTrie.zipper :: method.map(_._1)

        Step(remain, step.score + ((i + 1) -> sum))
      }

    out.score(message.length)
  }

}
