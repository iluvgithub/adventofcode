package com.myway.cats.rosienadam.scheduler

import cats.data.Chain
import cats.effect.{IO, Ref}
import com.myway.cats.rosienadam.core._

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

}
