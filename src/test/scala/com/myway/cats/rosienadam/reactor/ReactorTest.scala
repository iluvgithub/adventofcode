package com.myway.cats.rosienadam.reactor

import cats.Id
import cats.effect.kernel.Outcome
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import com.myway.cats.rosienadam.core.{BinaryStateMachine, JobId}
import com.myway.cats.rosienadam.scheduler.{JobScheduler, JobSchedulerState}
import munit.CatsEffectSuite

import scala.concurrent.duration.DurationInt

class ReactorTest extends CatsEffectSuite {

  test(" reactor when awake") {
    val maxRunning                     = 2
    val task                           = IO(42L)
    val state: JobSchedulerState[Long] = JobScheduler.JobSchedulerMemoryState[Long](maxRunning)
    val refStateIo: IO[Ref[IO, JobSchedulerState[Long]]] = Ref[IO].of(state)
    val actualIO: IO[Long] = for {
      ref    <- refStateIo
      refOut <- Ref[IO].of(-41L)
      scheduler = JobScheduler.makeScheduler(ref)
      reactor   = Reactor.fromStateRef[Long](ref)
      jobId <- scheduler.schedule(task)
      _ <- reactor.whenAwake(
        jid => IO(()),
        { case (jobId, outcome) =>
          for {
            actual <- outcome.fold(
              IO(Right(-1L)),
              e => IO(Left(e.getMessage)),
              _.map(Right(_))
            )
            _ <- actual.fold(_ => IO(()), refOut.set)
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
    ): (JobId, Outcome[IO, Throwable, Double]) => IO[Either[String, Double]] = {
      case (jobId, outcome) =>
        for {
          actual <- outcome.fold(
            IO(Left(s"Cancelled jobid=${jobId.id}")),
            e => IO(Left(e.getMessage)),
            _.map(Right(_))
          )
          _ <- actual.fold(_ => IO(()), refOut.set)
        } yield actual
    }

    val state: JobSchedulerState[Double] = JobScheduler.JobSchedulerMemoryState[Double](maxRunning)

    val actualIO: IO[Double] = for {
      refOut  <- Ref[IO].of[Double](-41.0)
      machine <- BinaryStateMachine.asleep
      ref     <- Ref[IO].of(state) // Ref[IO, JobScheduler.JobSchedulerState[String]]
      scheduler = JobScheduler.makeSchedulerSleep(ref, machine)
      reactor   = Reactor.fromStateRef[Double](ref)
      _ <- (
        IO.sleep(200.millis) >> scheduler.schedule(task),
        Reactor.loop(reactor, machine, onStart, onComplete(refOut))
      ).parTupled.timeout(600.millis).handleError(_ => IO())
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
