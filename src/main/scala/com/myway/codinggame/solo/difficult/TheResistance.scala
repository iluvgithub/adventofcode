package com.myway.codinggame.solo.difficult

object TheResistance {

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

  case class Bag[A](bagMap: Map[A, Long]) {

    def this() = this(Map[A, Long]())

    def this(a: A, l: Long) = this(Map[A, Long](a -> l))

    def this(a: A) = this(a, 1L)

    lazy val total: Long = this.bagMap.values.sum

    def addMany(a: A, l: Long): Bag[A] = Bag(bagMap.get(a) match {
      case None     => this.bagMap + (a     -> l)
      case Some(l0) => this.bagMap - a + (a -> (l + l0))
    })

    def add(a: A): Bag[A] = this.addMany(a, 1L)

    def get(a: A): Long = this.bagMap.getOrElse(a, 0L)

    def map[B](f: A => B): Bag[B] = flatMap(a => new Bag(f(a)))

    private def mapValues(f: Long => Long): Bag[A] =
      Bag(this.bagMap.toList.map { case (k, v) => (k, f(v)) }.toMap)

    def merge(that: Bag[A]): Bag[A] =
      that.bagMap.keySet.foldLeft(this)((bg, k) => bg.addMany(k, that.get(k)))

    def flatMap[B](g: A => Bag[B]): Bag[B] =
      this.bagMap.keySet.foldLeft[Bag[B]](new Bag())((bg, a) =>
        bg.merge(g(a).mapValues(_ * this.get(a)))
      )

    def cross[B](that: Bag[B]): Bag[(A, B)] = Bag((for {
      ak <- this.bagMap.toList
      bh <- that.bagMap.toList
    } yield ((ak._1, bh._1), ak._2 * bh._2)).toMap)

  }

  private def wordToMorse(s: String): String = s.toList.map(c => morseMap(c)).mkString
  def solve(l: List[String]): Long = {
    val message: String        = l.head
    val setWords: List[String] = l.tail.tail
    val setWordsMorse: Map[String, Int] =
      setWords.groupBy(wordToMorse).toList.map { case (k, v) => (k, v.size) }.toMap

    go(message, setWordsMorse)
  }

  def split(targetMessage: String): String => Option[(String, String, Int, String)] = candidate =>
    List
      .range(0, targetMessage.length)
      .map { j =>
        val idx = targetMessage.drop(j).indexOf(candidate)
        if (idx >= 0) Some(idx + j) else None
      }
      .filter(_.isDefined)
      .map(_.get)
      .map(i => (targetMessage.take(i), targetMessage.drop(i + candidate.length), i, candidate))
      .headOption

  private def hasPrefix(setWordsMorse: Map[String, Int]): String => Boolean =
    s => setWordsMorse.keySet.exists(w => w.length < s.length && s.startsWith(w))

  private def completeForAutoOverlap(
    left: String,
    right: String,
    idx: Int,
    candidate: String
  ): List[(String, String, Int, String)] =
    (left, right, idx, candidate) :: Nil

  private def go(targetMessage: String, setWordsMorse: Map[String, Int]): Long =
    if (targetMessage.isEmpty) 1L
    else if (setWordsMorse.contains(targetMessage) && !hasPrefix(setWordsMorse)(targetMessage))
      setWordsMorse(targetMessage)
    else {
      val splitInTheMiddle: List[(String, String, Int, String)] =
        setWordsMorse.keySet.toList
          .map(split(targetMessage))
          .filter(_.isDefined)
          .map(_.get)
          .sortBy(j => Math.abs(j._3 - (targetMessage.length / 2)))

      splitInTheMiddle match {
        case Nil => 0L
        case x :: xs =>
          val l: List[(String, String, Int, String)] = xs
            .foldLeft(List(x), x._3, x._3 + x._4.length) { (acc, u) =>
              val fromAcc = acc._2
              val toAcc   = acc._3
              val from    = u._3
              val to      = from + u._4.length
              val mini    = Math.min(to, toAcc)
              val maxi    = Math.max(from, fromAcc)
              if (maxi < mini)
                (u :: acc._1, maxi, mini)
              else
                acc
            }
            ._1

          val o = l
            .flatMap(t => completeForAutoOverlap(t._1, t._2, t._3, t._4))
            .map { case (left, right, i, w) =>
              go(left, setWordsMorse) * setWordsMorse(w) * go(right, setWordsMorse)
            }
          o.sum
      }

    }

}
