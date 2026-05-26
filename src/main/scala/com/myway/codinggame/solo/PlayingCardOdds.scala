package com.myway.codinggame.solo

object PlayingCardOdds {

  trait Parser[A] {
    def run(s: String): Option[(A, String)]

    def parse(s: String): Option[A]              = run(s).map(_._1)
    def map[B](f: A => B): Parser[B]             = Parser.map(this, f)
    def flatMap[B](g: A => Parser[B]): Parser[B] = Parser.flatMap(this, g)

    def or(that: Parser[A]): Parser[A] = Parser.or(this, that)

    def parseSure(s: String): A = parse(s).get
  }

  object Parser {
    def unit[A](a: A): Parser[A] = s => Some((a, s))

    def failure[A]: Parser[A] = _ => None

    def getchar: Parser[Char] = s => if (s.isEmpty) None else Some((s.charAt(0), s.tail))

    def sat(p: Char => Boolean): Parser[Char] = getchar.flatMap(c => if (p(c)) unit(c) else failure)

    def char(c: Char): Parser[Char]  = sat(_.equals(c))
    def char0(c: Char): Parser[Unit] = char(c).map(_ => ())

    def string(s: String): Parser[String] = s.foldLeft(unit(""))((acc, c) =>
      for {
        x <- acc
        _ <- char0(c)
      } yield s"$x$c"
    )
    def string0(s: String): Parser[Unit] = string(s).map(_ => ())

    def digit: Parser[Char] = sat(_.isDigit)
    def number: Parser[Long] = many(digit.map(c => s"$c".toInt))
      .map[Long](_.foldLeft[Long](0L)((acc, d) => acc * 10L + d))

    def lowers: Parser[String] = many(sat(_.isLower)).map(_.mkString)

    def map[A, B](p: Parser[A], f: A => B): Parser[B] = (s: String) =>
      p.run(s).map { case (a, s0) =>
        (f(a), s0)
      }
    def flatMap[A, B](p: Parser[A], g: A => Parser[B]): Parser[B] = (s: String) => {
      val o: Option[(A, String)] = p.run(s)
      o.flatMap { case (a, s0) => g(a).run(s0) }
    }

    def or[A](p: Parser[A], q: Parser[A]): Parser[A] = s =>
      p.run(s) match {
        case Some(x) => Some(x)
        case None    => q.run(s)
      }

    def many[A](p: Parser[A]): Parser[List[A]] = or(some(p), unit(Nil))
    def some[A](p: Parser[A]): Parser[List[A]] = for {
      x  <- p
      xs <- many(p)
    } yield x :: xs
  }

  val ranks: List[Char]  = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
  val colors: List[Char] = List('C', 'D', 'H', 'S')

  def solve(removed: List[String], sought: List[String]): String =
    s"${Math.round(solveD(removed, sought))}%"

  def solveD(removed: List[String], sought: List[String]): Double = {
    val rem: List[Card] = for {
      l <- removed
      x <- actualCards(l)
    } yield x

    val sou0: List[Card] = for {
      l <- sought
      x <- actualCards(l)
    } yield x

    val sou = sou0.filterNot(rem.contains)
    100.0 * (0.0 + sou.toSet.size) / (52.0 - rem.toSet.size)
  }

  def extractRanks(s: String): List[Char]  = extract(ranks, s)
  def extractColors(s: String): List[Char] = extract(colors, s)

  def extract(chars: List[Char], s: String): List[Char] =
    Parser.some(chr(chars)).parse(s).getOrElse(Nil)
  def chr(from: List[Char]): Parser[Char] = from.tail
    .foldLeft(Parser.char(from.head))((acc, c) => acc.or(Parser.char(c)))

  case class Card(rank: Char, color: Char)

  def actualCards(s: String): List[Card] = if (s.isEmpty) Nil
  else {
    val setRanks  = s.toList.intersect(ranks)
    val setColors = s.toList.intersect(colors)
    if (setRanks.isEmpty) {
      for {
        c <- setColors
        r <- ranks
      } yield Card(r, c)
    } else if (setColors.isEmpty) {
      for {
        c <- colors
        r <- setRanks
      } yield Card(r, c)
    } else if (setRanks.size <= 1) {
      for {
        c <- setColors
      } yield Card(setRanks.last, c)
    } else if (setColors.size <= 1) {
      for {
        r <- setRanks
      } yield Card(r, setColors.last)
    } else {
      for {
        c <- setColors
        r <- setRanks
      } yield Card(r, c)
    }

  }

}
