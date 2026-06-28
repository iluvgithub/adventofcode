package com.myway.cats.rosienadam.scheduler

import cats.Id
import cats.effect.kernel.Outcome
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import munit.CatsEffectSuite

class JobSchedulerTest extends CatsEffectSuite {

  test("with memory state ") {
    // arrange
    val maxRunning = 2
    val task       = IO("ABC")
    val actualIO: IO[String] = for {
      ref <- Ref[IO].of(
        JobScheduler.JobSchedulerMemoryState[String](maxRunning)
      ) // Ref[IO, JobScheduler.JobSchedulerMemoryState[String]]
      scheduler = JobScheduler.schedulerMem(ref)
      jobId     <- scheduler.schedule(task)
      scheduled <- ref.get.map(_.scheduled.headOption.get)
      running   <- scheduled.start
      o         <- running.await.flatMap(_.exitCase.fold(IO("cancel"), _ => IO(""), identity))
    } yield o
    // act

    for {
      control <- TestControl.execute(actualIO)
      _       <- control.tick
      _       <- control.results.assertEquals(Some(Outcome.succeeded[Id, Throwable, String]("ABC")))
    } yield ()

    // assert

  }

}
