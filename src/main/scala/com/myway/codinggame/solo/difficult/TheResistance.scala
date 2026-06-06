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
    solve(message, setWordsMorse)
  }

  private def solve(message: String, setWordsMorse0: Map[String, Int]): Long = if (message.isEmpty)
    1L
  else {
    val setWordsMorse = setWordsMorse0.toList.filter { case (k, _) => message.contains(k) }.toMap
    if (setWordsMorse.isEmpty) 0L
    else {
      val max = setWordsMorse.keySet.map(_.length).max
      if (message.length > 2 * max) {
        val mid0 = message.length / 2
        setWordsMorse.keySet.toList
          .flatMap { w =>
            List.range(0, w.length).map(j => (j, w))
          }
          .foldLeft(0L) { (o, pair) =>
            val key       = pair._2
            val j         = pair._1
            val left      = message.take(mid0 - j)
            val midString = message.drop(mid0 - j).take(key.length)
            val right     = message.drop(mid0 - j + key.length)
            o + solve(left, setWordsMorse) * setWordsMorse.getOrElse(midString, 0) * solveShort(
              right,
              setWordsMorse
            )
          }
      } else
        solveShort(message, setWordsMorse)
    }
  }

  private def solveShort(message: String, setWordsMorse: Map[String, Int]): Long = {

    val initBag = setWordsMorse.keySet.foldLeft(new Bag[String]().addMany("", 1L))((bg, k) =>
      bg.addMany(k, 0 + setWordsMorse(k))
    )
    val outBag = List.range(1, message.length + 1).foldLeft(initBag) { (bag, i) =>
      val w: String                     = message.take(i)
      val pairs: List[(String, String)] = split(w)
      pairs.foldLeft(bag) { (bg, pair) =>
        val o: Long = bag.get(pair._2) * setWordsMorse.getOrElse(pair._1, 0)
        if (o > 0L) bg addMany (w, o) else bg
      }
    }

    outBag.get(message)
  }

  def split(s: String): List[(String, String)] =
    List.range(1, s.length).map(i => (s.take(i), s.drop(i)))

}
