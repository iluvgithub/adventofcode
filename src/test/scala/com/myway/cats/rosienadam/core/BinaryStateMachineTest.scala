package com.myway.cats.rosienadam.core

import cats.Id
import cats.effect.kernel.Outcome
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import munit.CatsEffectSuite

import scala.concurrent.duration.DurationInt

class BinaryStateMachineTest extends CatsEffectSuite {

  test("asleep to awake ") {
    // arrange
    val actualIO: IO[Long] = for {
      machine <- BinaryStateMachine.asleep
      refOut  <- Ref[IO].of(-41L)
      _ <- (
        machine.sleep *> refOut.set(42L),
        IO.sleep(100.millis) >> machine.wakeUp
      ).parTupled.timeout(2.seconds)
      _ <- IO.sleep(200.millis)
      o <- refOut.get
    } yield o
    // act & assert
    for {
      control <- TestControl.execute(actualIO)
      _       <- control.tick
      _       <- control.advanceAndTick(200.millis)
      _       <- control.tick
      _       <- control.advanceAndTick(200.millis)
      _       <- control.results.assertEquals(Some(Outcome.succeeded[Id, Throwable, Long](42L)))

    } yield ()
  }

}
