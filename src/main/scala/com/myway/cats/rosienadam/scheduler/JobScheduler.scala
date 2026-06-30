package com.myway.cats.rosienadam.scheduler

import cats.data.Chain
import cats.effect._
import com.myway.cats.rosienadam.core.JobSyntax.{Completed, Running, Scheduled}
import com.myway.cats.rosienadam.core._

trait JobScheduler[A] {
  def schedule(task: IO[A]): IO[JobId]
}

trait JobSchedulerState[A] {

  def enqueue(job: Scheduled[A]): JobSchedulerState[A]
  def dequeue: (JobSchedulerState[A], Option[Scheduled[A]])

  def appendRunning(job: Running[A]): JobSchedulerState[A]

  def appendComplete(job: Completed[A]): JobSchedulerState[A]

  def findFirstScheduled : Option[Scheduled[A]]
}
object JobScheduler {
  case class JobSchedulerMemoryState[A](
    maxRunning: Int,
    scheduled: Chain[Scheduled[A]] = Chain.empty[Scheduled[A]],
    running: Map[JobId, Running[A]] = Map.empty[JobId, Running[A]],
    completed: Chain[Completed[A]] = Chain.empty[Completed[A]]
  ) extends JobSchedulerState[A] {

    def enqueue(job: Scheduled[A]): JobSchedulerState[A] =
      copy(scheduled = scheduled :+ job)

    def dequeue: (JobSchedulerState[A], Option[Scheduled[A]]) =
      scheduled.uncons
        .filter(_ => running.size < maxRunning)
        .map { case (head, tail) =>
          copy(scheduled = tail) -> Some(head)
        }
        .getOrElse(this -> None)
    def appendRunning(job: Running[A]): JobSchedulerMemoryState[A] =
      copy(running = running + (job.jobId -> job))

    def appendComplete(job: Completed[A]): JobSchedulerMemoryState[A] =
      copy(completed = completed :+ job, running = running - job.jobId)

    override def findFirstScheduled: Option[Scheduled[A]] = scheduled.headOption
  }

  def makeScheduler[A](ref: Ref[IO, JobSchedulerState[A]]): JobScheduler[A] =
    (task: IO[A]) =>
      for {
        job <- JobSyntax.create(task)
        _   <- ref.update(_.enqueue(job))
      } yield job.jobId

  def makeSchedulerSleep[A](
    ref: Ref[IO, JobSchedulerState[A]],
    zzz: BinaryStateMachine
  ): JobScheduler[A] =
    (task: IO[A]) =>
      for {
        jobId <- makeScheduler(ref).schedule(task)
        _     <- zzz.wakeUp
      } yield jobId

}
