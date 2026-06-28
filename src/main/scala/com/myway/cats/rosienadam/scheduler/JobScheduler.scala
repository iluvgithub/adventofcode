package com.myway.cats.rosienadam.scheduler

import cats.data.Chain
import cats.effect.{IO, Outcome, Ref, Resource}
import cats.implicits.toFunctorOps
import com.myway.cats.rosienadam.core._
import com.myway.cats.rosienadam.reactor.Reactor

trait JobScheduler[A] {
  def schedule(task: IO[A]): IO[JobId]
}

object JobScheduler {
  case class JobSchedulerMemoryState[A](
    maxRunning: Int,
    scheduled: Chain[Scheduled[A]] = Chain.empty[Scheduled[A]],
    running: Map[JobId, Running[A]] = Map.empty[JobId, Running[A]],
    completed: Chain[Completed[A]] = Chain.empty[Completed[A]]
  ) {

    def enqueue(job: Scheduled[A]): JobSchedulerMemoryState[A] =
      copy(scheduled = scheduled :+ job)

    def dequeue: (JobSchedulerMemoryState[A], Option[Scheduled[A]]) =
      scheduled.uncons
        .filter(_ => running.size < maxRunning)
        .map { case (head, tail) =>
          copy(scheduled = tail) -> Some(head)
        }
        .getOrElse(this -> None)
    def appendRunning(job: Running[A]): JobSchedulerMemoryState[A] =
      copy(running = running + (job.jobId -> job))

    def onComplete(job: Completed[A]): JobSchedulerMemoryState[A] =
      copy(completed = completed :+ job, running = running - job.jobId)
  }

  def schedulerMem[A](ref: Ref[IO, JobSchedulerMemoryState[A]]): JobScheduler[A] =
    (task: IO[A]) =>
      for {
        job <- JobSyntax.create(task)
        _   <- ref.update(_.enqueue(job))
      } yield job.jobId

  def schedulerMemSleep[A](
    ref: Ref[IO, JobSchedulerMemoryState[A]],
    zzz: BinaryStateMachine
  ): JobScheduler[A] =
    (task: IO[A]) =>
      for {
        jobId <- schedulerMem(ref).schedule(task)
        _     <- zzz.wakeUp
      } yield jobId

}

object JobSchedulerWithBinaryMachine {

  import com.myway.cats.rosienadam.scheduler.JobScheduler.JobSchedulerMemoryState
  def resource[A](maxRunning: Int): IO[Resource[IO, JobScheduler[A]]] =
    for {
      stateRef       <- IO(JobSchedulerMemoryState[A](maxRunning))
      schedulerState <- Ref[IO].of(stateRef)
      machine        <- BinaryStateMachine.asleep
      scheduler = JobScheduler.schedulerMemSleep(schedulerState, machine)
      reactor   = Reactor.fromStateRef[A](schedulerState)
      onStart0  = (id: JobId) => IO.unit
      onComplete0 = (id: JobId, outcome: Outcome[IO, Throwable, A]) =>
        outcome.fold[IO[Either[String, A]]](
          IO(Left(s"Cancelled job id:$id")),
          e => IO(Left(e.getMessage)),
          _.map(Right(_))
        )
      loop = Reactor.loop(reactor, machine, onStart0, onComplete0)

    } yield loop.background.as(scheduler)
}
