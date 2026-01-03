package com.myway.adventofcode.tools.list

object ListUtil {

  def cpl[A, B](a: A, bs: List[B]): List[(A, B)] = bs.map(b => (a, b))

  def cpr[A, B](as: List[A], b: B): List[(A, B)] = as.map(a => (a, b))

  def cpp[A, B](as: List[A], bs: List[B]): List[(A, B)] = for {
    a <- as
    b <- bs
  } yield (a, b)

  def cpList[A](as: List[List[A]]): List[List[A]] = as.reverse.foldLeft[List[List[A]]](Nil :: Nil)(
    (acc, xs) => cpp(xs, acc).map({ case (u, vs) => u :: vs })
  )

}
