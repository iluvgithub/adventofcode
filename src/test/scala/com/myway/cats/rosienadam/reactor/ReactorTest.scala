package com.myway.cats.rosienadam.reactor

import cats.Id
import cats.effect.kernel.Outcome
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import com.myway.cats.rosienadam.core.{BinaryStateMachine, JobId}
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
      reactor   = Reactor.fromStateRef[Long](ref)
      jobId <- scheduler.schedule(task)
      _ <- reactor.whenAwake(
        jid => IO(()),
        { case (jobId, outcome) =>
          for {
            actual <- outcome.fold(
              IO(-1L),
              _ => IO(-2L),
              identity
            )
            _ <- refOut.set(actual)
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

  test("reactor with machine") {
    // arrange
    val maxRunning                 = 2
    val task: IO[Double]           = IO(3.14)
    val onStart: JobId => IO[Unit] = jid => IO(())

    def onComplete(
      refOut: Ref[IO, Double]
    ): (JobId, Outcome[IO, Throwable, Double]) => IO[Double] = { case (jobId, outcome) =>
      for {
        actual <- outcome.fold(
          IO(-1.0),
          _ => IO(-2.0),
          identity
        )
        _ <- refOut.set(actual)
      } yield actual
    }

    val actualIO: IO[Double] = for {
      refOut  <- Ref[IO].of[Double](-41.0)
      machine <- BinaryStateMachine.asleep
      ref <- Ref[IO].of(
        JobScheduler.JobSchedulerMemoryState[Double](maxRunning)
      ) // Ref[IO, JobScheduler.JobSchedulerMemoryState[String]]
      scheduler = JobScheduler.schedulerMemSleep(ref, machine)
      reactor   = Reactor.fromStateRef[Double](ref)
      _ <- (
        IO.sleep(200.millis) >> scheduler.schedule(task),
        Reactor.loop(reactor, machine, onStart, onComplete(refOut))
      ).parTupled.timeout(600.millis).handleError(_=>IO())
      _ <- IO.sleep(200.millis)
      d <- refOut.get
    } yield d

    // act && assert

    for {
      control <- TestControl.execute(actualIO)
      _       <- control.tick
      _       <- control.advanceAndTick(500.millis)
      _       <- control.tick
      _       <- control.advanceAndTick(500.millis)
      _       <- control.tick
      _       <- control.advanceAndTick(500.millis)
      _       <- control.results.assertEquals(Some(Outcome.succeeded[Id, Throwable, Double](3.14)))
    } yield ()

  }
}
