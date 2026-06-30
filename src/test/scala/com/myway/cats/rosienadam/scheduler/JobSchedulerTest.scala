package com.myway.cats.rosienadam.scheduler

import cats.Id
import cats.effect.kernel.Outcome
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import com.myway.cats.rosienadam.core.JobSyntax.Scheduled
import com.myway.cats.rosienadam.core.Scheduled
import munit.CatsEffectSuite

class JobSchedulerTest extends CatsEffectSuite {

  test("with memory state ") {
    // arrange
    val maxRunning                       = 2
    val task                             = IO("ABC")
    val state: JobSchedulerState[String] = JobScheduler.JobSchedulerMemoryState[String](maxRunning)
    val refIo: IO[Ref[IO, JobSchedulerState[String]]] =Ref[IO].of(state)
    val actualIO: IO[String] = for {
      ref <- refIo
      scheduler = JobScheduler.makeScheduler(ref)
      jobId     <- scheduler.schedule(task)
      scheduled  <- ref.get.map[Scheduled[String]](_.findFirstScheduled.get)
      running   <- scheduled.start
      o         <- running.await.flatMap(_.exitCase.fold(IO("cancel"), _ => IO(""), identity))
    } yield o
    // act && assert

    for {
      control <- TestControl.execute(actualIO)
      _       <- control.tick
      _       <- control.results.assertEquals(Some(Outcome.succeeded[Id, Throwable, String]("ABC")))
    } yield ()

  }

}
