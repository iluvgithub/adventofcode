package com.myway.cats.tagless.expr.`final`.algebraicui

import com.myway.cats.tagless.expr.`final`.algebraicui.Controls.{Validation, succeed}

trait Algebra[F[_]]

trait Controls[F[_]] extends Algebra[F]{
  def textInput(
                 label: String,
                 placeholder: String,
                 validation: Validation[String] = succeed
               ): F[String]
  def choice[A](label: String, options: Seq[(String, A)]): F[A]
}
trait Layout[F[_]] extends Algebra[F]{
  def and[A, B](first: F[A], second: F[B]): F[(A, B)]
}


object Controls {
  type Validation[A] = A => Either[String, A]
  def succeed[A](value: A): Either[String, A] = Right(value)
}
/*
import com.myway.cats.tagless.expr.`final`.algebraicui.Controls.{Validation, succeed}

trait Controls[F[_]] {
  def textInput(
                 label: String,
                 placeholder: String,
                 validation: Validation[String] = succeed
               ): F[String]
  def choice[A](label: String, options: Seq[(String, A)]): F[A]
}

object Controls {
  type Validation[A] = A => Either[String, A]
  def succeed[A](value: A): Either[String, A] = Right(value)
}

trait Layout[F[_]] {
  def and[A, B](first: F[A], second: F[B]): F[(A, B)]
}

 */