package com.myway.cats.rosienadam.reactor

import cats.Id
import cats.effect.kernel.Outcome
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import com.myway.cats.rosienadam.scheduler.JobScheduler
import munit.CatsEffectSuite

import scala.concurrent.duration.DurationInt

class ReactorTest extends CatsEffectSuite {

  test(" reactor when awake") {
    val maxRunning = 2
    val task       = IO(42L)

    val actualIO: IO[Long] = for {
      ref <- Ref[IO].of(
        JobScheduler.JobSchedulerMemoryState[Long](maxRunning)
      ) // Ref[IO, JobScheduler.JobSchedulerMemoryState[String]]
      refOut <- Ref[IO].of(-41L)
      scheduler = JobScheduler.schedulerMem(ref)
      reactor   = Reactor.apply[Long](ref)
      jobId <- scheduler.schedule(task)
      _ <- reactor.whenAwake(
        jid => IO(assertEquals(jid, jobId)),
        { case (jobId, outcome) =>
          for {
            actual <- outcome.fold(
              IO(-1L),
              _ => IO(-2L),
              identity
            )
            _ <-  refOut.set(actual)
          } yield actual
        }
      )
      _ <- IO.sleep(50.millis)
      l <- refOut.get
    } yield l

    // act && assert

    for {
      control <- TestControl.execute(actualIO)
      _       <- control.tick
      _       <- control.advanceAndTick(100.millis)
      _       <- control.results.assertEquals(Some(Outcome.succeeded[Id, Throwable, Long](42L)))
    } yield ()

  }

}
