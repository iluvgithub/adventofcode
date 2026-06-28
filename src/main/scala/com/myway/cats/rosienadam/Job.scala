package com.myway.cats.rosienadam

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.{Deferred, Outcome}
import cats.effect.{Fiber, IO}

import java.util.UUID

trait Job

case class Scheduled[A](jobId: JobId, task: IO[A]) extends Job {
  def start: IO[Running[A]] =
    for {
      exitCase <- Deferred[IO, Outcome[IO, Throwable, A]]
      fiber <- task
        .guaranteeCase(x =>
          exitCase
            .complete(x match {
              case Succeeded(fa) => Outcome.succeeded(fa)
              case Errored(t)    => Outcome.errored(t)
              case Canceled()    => Outcome.canceled
            })
            .void
        )
        .start
    } yield Running(jobId, fiber, exitCase)
}

case class Running[A](
  jobId: JobId,
  fiber: Fiber[IO, Throwable, A],
  exitCase: Deferred[IO, Outcome[IO, Throwable, A]]
) extends Job {
  val await: IO[Completed[A]] =
    exitCase.get.map(Completed(jobId, _))

  def cancel : IO[Unit] = fiber.cancel
}

case class Completed[A](jobId: JobId, exitCase: Outcome[IO, Throwable, A]) extends Job

object JobSyntax {
  def create[A](task: IO[A]): IO[Scheduled[A]] =
    IO(JobId(UUID.randomUUID().toString)).map(Scheduled(_, task))
}
