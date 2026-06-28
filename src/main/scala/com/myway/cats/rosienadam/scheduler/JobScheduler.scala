package com.myway.cats.rosienadam.scheduler

import cats.data.Chain
import cats.effect.{IO, Ref, Resource}
import com.myway.cats.rosienadam.core._

trait JobScheduler[A] {
  def schedule(task: IO[A]): IO[JobId]
}

object JobScheduler {
  case class JobSchedulerMemoryState[A](
                       maxRunning: Int,
                       scheduled: Chain[Scheduled[A]] = Chain.empty[Scheduled[A]],
                       running: Map[JobId, Running[A]] = Map.empty[JobId,Running[A]],
                       completed: Chain[Completed[A]] = Chain.empty[Completed[A]]
  ) {

    def enqueue(job: Scheduled[A]): JobSchedulerMemoryState[A] = copy(scheduled = scheduled :+ job)

  }

  def schedulerMem[A](ref: Ref[IO, JobSchedulerMemoryState[A]]):JobScheduler[A] =
    (task: IO[A]) => for {
      job <- JobSyntax.create(task)
      _ <- ref.update(_.enqueue(job))
    } yield job.jobId


}