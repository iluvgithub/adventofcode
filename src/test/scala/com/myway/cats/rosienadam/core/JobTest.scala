package com.myway.cats.rosienadam.core

import cats.Id
import cats.effect.testkit.TestControl
import cats.effect.{IO, Outcome}
import munit.CatsEffectSuite

import scala.concurrent.duration.DurationInt
class JobTest extends CatsEffectSuite {

  test(" start scheduled ok") {
    // arrange
    val io: IO[Int]           = IO(1)
    val sched: Scheduled[Int] = Scheduled(JobId("abc"), io)

    val ioCompleted: IO[Int] = for {
      running   <- sched.start
      completed <- running.await
      out       <- completed.exitCase.fold(IO(-1), e => IO(-2), identity)
    } yield out
    // act
    for {
      control <- TestControl.execute(ioCompleted)
      _       <- control.tick
      _       <- control.advanceAndTick(1.second)
      // assert
      _ <- control.results.assertEquals(
        Some(Outcome.succeeded[Id, Throwable, Int](1))
      )
    } yield ()

  }

  test(" start scheduled not ok") { // arrange
    val io: IO[Int]           = IO.raiseError[Int](new RuntimeException("Some error occurred"))
    val sched: Scheduled[Int] = Scheduled(JobId("abc"), io)

    val ioCompleted: IO[String] = for {
      running   <- sched.start
      completed <- running.await
      out       <- completed.exitCase.fold(IO( "cancelled"), e => IO(e.getMessage),  _.map(_=>"success"))
    } yield out
    // act
    for {
      control <- TestControl.execute(ioCompleted)
      _       <- control.tick
      _       <- control.advanceAndTick(1.second)
      // assert
      _ <- control.results.assertEquals(
        Some(Outcome.succeeded[Id, Throwable, String]("Some error occurred"))
      )
    } yield ()
  }

  test(" start scheduled cancelled") {
    // arrange
    val io: IO[Int]           = IO.sleep(1.second) >> IO(1)
    val sched: Scheduled[Int] = Scheduled(JobId("abc"), io)

    val ioCompleted: IO[Int] = for {
      running   <- sched.start
      _ <- IO.sleep(50.millis) >> running.cancel
      completed <- running.await
      out       <- completed.exitCase.fold(IO(-1), _ => IO(-2), identity)
    } yield out
    // act
    for {
      control <- TestControl.execute(ioCompleted)
      _       <- control.tick
      _       <- control.advanceAndTick(100.millis)
      // assert
      _ <- control.results.assertEquals(
        Some(Outcome.succeeded[Id, Throwable, Int](-1))
      )
    } yield ()

  }
}
