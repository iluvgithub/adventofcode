package com.myway.adventofcode.tools.list

object ListUtil {

  def cpl[A, B](a: A, bs: List[B]): List[(A, B)] = bs.map(b => (a, b))

  def cpr[A, B](as: List[A], b: B): List[(A, B)] = as.map(a => (a, b))

  def cpp[A, B](as: List[A], bs: List[B]): List[(A, B)] = for {
    a <- as
    b <- bs
  } yield (a, b)

  def cpList[A](as: List[List[A]]): List[List[A]] =
    as.reverse.foldLeft[List[List[A]]](Nil :: Nil)((acc, xs) =>
      cpp(xs, acc).map { case (u, vs) => u :: vs }
    )

  def permutations[A](l: List[A]): List[List[A]] = l.foldLeft[List[List[A]]](List(Nil)) {
    (acc, a) =>
      for {
        (a0, as) <- cpl(a, acc)
        (l, r)   <- splits(as)
      } yield l ++ (a0 :: r)
  }

  def splits[A](as: List[A]): List[(List[A], List[A])] =
    List.range(0, as.size + 1).map(i => (as.take(i), as.drop(i)))

  def maxSegSum(l: List[Int]): List[Int] = {
    val o: List[(Int, List[Int])] =  l.scanLeft[(Int,List[Int])]((0,Nil))(
      (acc, i) => {
        val sum: Int = acc._1 + i
        if(sum < 0) (0,Nil) else {
          (sum, i :: acc._2)
        }

      }
    )
    o.maxBy(_._1)._2.reverse
  }

}
