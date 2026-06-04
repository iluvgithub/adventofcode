package com.myway.adventofcode.tools.free
import scala.annotation.tailrec

// Free<X> = X + F<Free<X>>
sealed trait Free[F[_], A] {

  final def resume(implicit F: Functor[F]): Either[F[Free[F, A]], A] = this match {
    case d: Done[F, A]        => Right(d.a)
    case m: More[F, A]        => Left(m.k)
    case fm: FlatMap[F, _, A] => fm.transform(F)
  }

  def flatMap[B](g: A => Free[F, B]): Free[F, B] = FlatMap(this, g)

  def map[B](f: A => B): Free[F, B] = flatMap(f.andThen(Done[F, B]))

}

case class Done[F[_], A](a: A) extends Free[F, A]

case class More[F[_], A](k: F[Free[F, A]]) extends Free[F, A]

private case class FlatMap[F[_], A, B](
  a: Free[F, A],
  k: A => Free[F, B]
) extends Free[F, B] {

  def transform(implicit F: Functor[F]): Either[F[Free[F, B]], B] = a match {
    case d: Done[F, A]        => k(d.a).resume
    case m: More[F, A]        => Left(F.map(m.k)(_ flatMap k))
    case fm: FlatMap[F, _, A] => fm.transform(k)(F)

  }

  private def transform[C](f: B => Free[F, C])(implicit F: Functor[F]): Either[F[Free[F, C]], C] =
    a.flatMap(x => k(x).flatMap(f)).resume
}
object Free {

  type Trampoline[A] = Free[Function0, A]

  implicit val FUN: Functor[Function0] = new Functor[Function0]() {

    override def map[A, B](fa: () => A)(f: A => B): () => B =
      () => f(fa())
  }
  @tailrec
  def solve[A](tr: Trampoline[A]): A = tr.resume match {
    case Left(f0) => solve(f0.apply())
    case Right(a) => a
  }
  implicit class Bouncer[A](tr: Trampoline[A]) {
    def run: A = solve(tr)

  }

  def done[A](a: A): Trampoline[A] = Done[Function0, A](a)

  def more[A](k: () => Trampoline[A]): Trampoline[A] = More[Function0, A](k)

  type Pair[A]    = (A, A)
  type BinTree[A] = Free[Pair, A]

  type Tree[A] = Free[List, A]

  type FreeList[A] = Free[({ type λ[+α] = (A, α) })#λ, Unit]
}
