package com.myway.adventofcode.tools.category

trait Functor[F[A]] {

  def map[A, B](fa: F[A], f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa, f)

}

object FunctorObj {

  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] =
      F.map(fa, f)
  }
}