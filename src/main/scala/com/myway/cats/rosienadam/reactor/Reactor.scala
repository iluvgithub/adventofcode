package com.myway.cats.rosienadam.reactor

import cats.effect.{FiberIO, IO, Outcome, Ref}
import cats.implicits.toTraverseOps
import com.myway.cats.rosienadam.core.JobSyntax.{Completed, Running, Scheduled}
import com.myway.cats.rosienadam.core._
import com.myway.cats.rosienadam.scheduler.JobSchedulerState

trait Reactor[A] {
  def whenAwake(
    onStart: JobId => IO[Unit],
    onComplete: (JobId, Outcome[IO, Throwable, A]) => IO[Either[String, A]]
  ): IO[Unit]
}

object Reactor {
  def fromStateRef[A](stateRef: Ref[IO, JobSchedulerState[A]]): Reactor[A] =
    (
      onStart: JobId => IO[Unit],
      onComplete: (JobId, Outcome[IO, Throwable, A]) => IO[Either[String, A]]
    ) => {

      def startNextJob: IO[Option[Running[A]]] =
        for {
          optJob  <- stateRef.modify(_.dequeue)
          running <- optJob.traverse(startJob)
        } yield running

      def startJob: Scheduled[A] => IO[Running[A]] = scheduled =>
        for {
          running <- scheduled.start
          _       <- stateRef.update(_.appendRunning(running))
          _       <- registerOnComplete(running)
          _       <- onStart(running.jobId).attempt
        } yield running

      def registerOnComplete(job: Running[A]): IO[FiberIO[Unit]] =
        job.await
          .flatMap(jobCompleted)
          .start

      def jobCompleted(job: Completed[A]): IO[Unit] =
        stateRef
          .update(_.appendComplete(job))
          .flatTap(_ => onComplete(job.jobId, job.exitCase).attempt)

      // stateRef.get.map(stateRef => print(s"sz=${stateRef.running.size}, ")) >>
      startNextJob.iterateUntil(_.isEmpty).void
    }

  def loopStep[A](
    reactor: Reactor[A],
    machine: BinaryStateMachine,
    onStart: JobId => IO[Unit],
    onComplete: (JobId, Outcome[IO, Throwable, A]) => IO[Either[String, A]]
  ): IO[Unit] =
    machine.sleep >> reactor.whenAwake(onStart, onComplete)

  def loop[A](
    reactor: Reactor[A],
    machine: BinaryStateMachine,
    onStart: JobId => IO[Unit],
    onComplete: (JobId, Outcome[IO, Throwable, A]) => IO[Either[String, A]]
  ): IO[Nothing] =
    loopStep(reactor, machine, onStart, onComplete).foreverM

}
