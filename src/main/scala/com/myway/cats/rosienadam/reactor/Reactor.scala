package com.myway.cats.rosienadam.reactor

import cats.effect.{FiberIO, IO, Outcome, Ref}
import cats.implicits.toTraverseOps
import com.myway.cats.rosienadam.core._
import com.myway.cats.rosienadam.scheduler.JobScheduler.JobSchedulerMemoryState

trait Reactor[A] {
  def whenAwake(
    onStart: JobId => IO[Unit],
    onComplete: (JobId, Outcome[IO, Throwable, A]) => IO[A]
  ): IO[Unit]
}

object Reactor {
  def fromStateRef[A](stateRef: Ref[IO, JobSchedulerMemoryState[A]]): Reactor[A] =
    (onStart: JobId => IO[Unit], onComplete: (JobId, Outcome[IO, Throwable, A]) => IO[A]) => {

      def startNextJob: IO[Option[Running[A]]] =
        for {
          job     <- stateRef.modify(_.dequeue)
          running <- job.traverse(startJob)
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
          .update(_.onComplete(job))
          .flatTap(_ => onComplete(job.jobId, job.exitCase).attempt)

      startNextJob.iterateUntil(_.isEmpty).void
    }

  def loop[A]( reactor: Reactor[A],
               machine: BinaryStateMachine,
               onStart: JobId => IO[Unit],
               onComplete: (JobId, Outcome[IO, Throwable, A]) => IO[A]):IO[Nothing] =
    ( machine.sleep *>  reactor.whenAwake(onStart,onComplete)).foreverM

}
