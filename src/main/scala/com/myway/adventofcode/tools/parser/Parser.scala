package com.myway.adventofcode.tools.parser

import com.myway.adventofcode.tools.category.Monad

import scala.annotation.tailrec

trait Parser[M[_], A] {
  self =>

  def runParser(s: String)(implicit MON: Monad[M]): M[A] =
    MON.map[(A, String), A](run(s), _._1)

  def run(s: String): M[(A, String)]

  def map[B](f: A => B)(implicit MON: Monad[M]): Parser[M, B] = (s: String) => MON.map[(A, String), (B, String)](
    self.run(s), { case (x, s) => (f(x), s) }
  )

  def flatMap[B](g: A => Parser[M, B])(implicit MON: Monad[M]): Parser[M, B] =
    (s: String) => MON.flatMap[(A, String), (B, String)](
      self.run(s),
      { case (a, rest) => g(a).run(rest) }
    )
}


object ParserErrorMonad {

  type Error[X] = Either[String, X]
  type ParserError[X] = Parser[Error, X]

  def getchar: ParserError[Char] = new ParserError[Char]() {

    override def run(s: String): Error[(Char, String)] =
      if (s.isEmpty) Left("Empty String")
      else Right((s.charAt(0), s.tail))
  }

  implicit val MON: Monad[Error] = new Monad[Error] {
    override def unit[A](a: A): Error[A] = Right(a)

    override def flatMap[A, B](fa: Error[A], g: A => Error[B]): Error[B] =
      fa.fold(Left(_), g)

    override def tailRecM[A, B](init: A)(fn: A => Error[Either[A, B]]): Error[B] = ???

    override def map[A, B](fa: Error[A], f: A => B): Error[B] =
      flatMap[A, B](fa, f andThen unit)
  }

  implicit class ParserErrorWrapper[A](p: ParserError[A]) {
    def parse(s: String): Error[A] =
      p.runParser(s)
  }

  def fail[A](msg: String): ParserError[A] = (_: String) => Left(msg)

  implicit val ERROR_PARSER_MON: Monad[ParserError] = new Monad[ParserError] {

    override def unit[A](a: A): ParserError[A] = new ParserError[A]() {

      override def run(s: String): Error[(A, String)] = Right((a, s))
    }

    override def flatMap[A, B](fa: ParserError[A], g: A => ParserError[B]): ParserError[B] =
      fa.flatMap(g)

    override def tailRecM[A, B](init: A)(fn: A => ParserError[Either[A, B]]): ParserError[B] = ???

    override def map[A, B](fa: ParserError[A], f: A => B): ParserError[B] = fa.map(f)

  }

  def sat(p: Char => Boolean): ParserError[Char] =
    ERROR_PARSER_MON.flatMap[Char, Char](getchar,
      x => if (p(x)) ERROR_PARSER_MON.unit(x) else fail(s"[$x] failed test")
    )

  def char(c: Char): ParserError[Char] = sat(_.equals(c))

  def char0(c: Char): ParserError[Unit] = char(c).map(_ => ())

  def string(s: String): ParserError[String] =
    s.foldLeft(ERROR_PARSER_MON.unit(""))(
      (p, c) => for {
        s <- p
        x <- char(c)
      } yield s"$s$x"
    )

  def digit: ParserError[Int] = sat(_.isDigit).map(x => s"$x".toInt)

  def number: ParserError[Long] = many(digit)
    .map[Long](_.foldLeft[Long](0L)((acc, d) => acc * 10L + d))

  private def intoNumber(l: List[Int]): Long = l.foldLeft(0L)((acc, d) => acc * 10L + d)

  def string0(s: String): ParserError[Unit] = string(s).map(_ => ())

  def many[A](p: ParserError[A]): ParserError[List[A]] = new ParserError[List[A]]() {

    override def run(input: String): Error[(List[A], String)] = {
      @tailrec
      def loop(inp: String, acc: List[A]): Error[(List[A], String)] = {
        if (inp.isEmpty) Right((acc.reverse, ""))
        else p.run(inp) match {
          case Right((a, rest)) =>
            if (rest.length.equals(inp.length)) Left("Infinite loop")
            else loop(rest, a :: acc)
          case Left(_) => Right((acc.reverse, inp))
        }
      }

      loop(input, Nil)
    }

  }

  def orElse[A](p: ParserError[A], other: ParserError[A]): ParserError[A] =
    (s: String) => p.run(s).fold(
      _ => other.run(s),
      MON.unit
    )

  def token[A](p: ParserError[A]): ParserError[A] = for {
    _ <- many(char(' '))
    out <- p
  } yield out
}