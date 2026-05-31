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
    val message: String                         = l.head
    val setWords: Set[String]                   = l.tail.tail.toSet
    val setWordsMorse: Map[String, Set[String]] = setWords.groupBy(wordToMorse)

    val bag: Bag[String] =
      List.range(1, message.length + 1).foldLeft(new Bag[String]()) { (acc0, i) =>
        val prefix = message.take(i)
        val pairs: List[(String, String)] = split(prefix)
          .filter(w => setWordsMorse.contains(w._2))

        val acc =
          if (setWordsMorse.contains(prefix)) {
            acc0.addMany(prefix, setWordsMorse.getOrElse(prefix, Set()).size)
          } else acc0
        val out =
          pairs.foldLeft(acc) { (o, pair) =>
            val m =
              if (setWordsMorse.contains(pair._2)) setWordsMorse.getOrElse(pair._2, Set()).size
              else acc.get(pair._2)
            o.addMany(prefix, acc.get(pair._1) * m)
          }
        out
      }
    bag.get(message)

  }

  def split(s: String): List[(String, String)] =
    List
      .range(1, s.length)
      .foldLeft[List[(String, String)]](Nil)((acc, i) =>
        (s.take(s.length - i), s.drop(s.length - i)) :: acc
      )

}
