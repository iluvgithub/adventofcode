package com.myway.adventofcode.tools.category

import scala.annotation.tailrec

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: A): M[A]

  def flatMap[A, B](fa: M[A], g: A => M[B]): M[B]

  def tailRecM[A, B](init: A)(fn: A => M[Either[A, B]]): M[B]
}

object Monad {

  type Id[X] = X

  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](fa: Id[A], g: A => Id[B]): Id[B] = g(fa)

    override def map[A, B](fa: Id[A], f: A => B): Id[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](init: A)(fn: A => Id[Either[A, B]]): Id[B] =
      fn(init) match {
        case Left(a) => tailRecM(a)(fn)
        case Right(b) => b
      }

    override def unit[A](a: A): Id[A] = a
  }
}