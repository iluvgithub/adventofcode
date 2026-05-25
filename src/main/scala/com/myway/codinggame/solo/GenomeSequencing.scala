package com.myway.codinggame.solo

object GenomeSequencing {

  def cpl[A, B](a: A, bs: List[B]): List[(A, B)] = bs.map(b => (a, b))

  def permutations[A](l: List[A]): List[List[A]] = l.foldLeft[List[List[A]]](List(Nil)) {
    (acc, a) =>
      for {
        (a0, as) <- cpl(a, acc)
        (l, r)   <- splits(as)
      } yield l ++ (a0 :: r)
  }

  def splits[A](as: List[A]): List[(List[A], List[A])] =
    List.range(0, as.size + 1).map(i => (as.take(i), as.drop(i)))

  def cat(l: String, r: String): String =
    if (l.contains(r)) l
    else if (r.contains(l)) r
    else {
      val mn = Math.min(l.length, r.length)
      val m  = List.range(0, mn).findLast(i => l.takeRight(i).equals(r.take(i))).getOrElse(0)
      l.dropRight(m) ++ r
    }

  def solve(l: List[String]): String =
    permutations(l)
      .map(_.foldLeft("")(cat))
      .minBy(_.length)

}
