package com.myway.adventofcode.tools.category

trait Functor[F[_]] {

  def map[A, B](fa: F[A], f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa, f)

}

object FunctorObj {

  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] =
      F.map(fa, f)
  }
}

trait Filterable[F[_]] {
  def withFilter[A](fa: F[A], p: A => Boolean): F[A]
}

object FilterableObj {

  implicit class FilterOps[F[_], A](fa: F[A])(implicit F: Filterable[F], G: Functor[F]) {

    def map[B](f: A => B): F[B]           = G.map(fa, f)
    def withFilter(p: A => Boolean): F[A] = F.withFilter(fa, p)
  }
}
