package com.myway.adventofcode.tools.category

trait Functor[F[_]] {

  def map[A, B](fa: F[A], f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = fa => map(fa, f)

  def inflate[A](fa: F[A]): F[Option[A]] = map(fa, Some(_))
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

trait FilterableFunctor[F[_]] extends Functor[F] with Filterable[F] {

  def deflate[A](foa: F[Option[A]]): F[A] =
    map[Option[A], A](withFilter[Option[A]](foa, _.isDefined), _.get)
}

object FilterableObj {

  implicit class FilterOps[F[_], A](fa: F[A])(implicit F: FilterableFunctor[F]) {

    def map[B](f: A => B): F[B]           = F.map(fa, f)
    def withFilter(p: A => Boolean): F[A] = F.withFilter(fa, p)

  }

  def filter[F[_]: FilterableFunctor, A](p: A => Boolean): F[A] => F[A] =
    fa =>
      implicitly[FilterableFunctor[F]].deflate(
        implicitly[Functor[F]].inflate(fa).map(_.filter(p))
      )

  implicit val eitherFilterableFunctor: FilterableFunctor[({type λ[+α] = Either[String, α]})#λ] =
    new FilterableFunctor[({ type λ[+α] = Either[String, α] })#λ] {

      override def withFilter[A](fa: Either[String, A], p: A => Boolean): Either[String, A] =
        fa match {
          case Left(s)  => Left(s)
          case Right(a) => if (p(a)) Right(a) else Left(s"$a does not pass test")
        }

      override def map[A, B](fa: Either[String, A], f: A => B): Either[String, B] = fa match {
        case Left(s)  => Left(s)
        case Right(a) => Right(f(a))
      }
    }
}
