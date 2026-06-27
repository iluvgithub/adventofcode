package com.myway.cats.effect

import cats.Id
import cats.effect.testkit.TestControl
import cats.effect.{IO, Outcome}
import munit.CatsEffectSuite

import scala.concurrent.duration.DurationInt

class SampleIoTest extends CatsEffectSuite {

  test("io ") {
    val io: IO[Int] = IO(4).flatMap(x => IO.sleep(500.millis) >> IO(3 + x))

    for {
      control <- TestControl.execute(io)
      _       <- control.results.assertEquals(None)
      _       <- control.tick
      _       <- control.results.assertEquals(None)
      _       <- control.advanceAndTick(1.second)
      _       <- control.results.assertEquals(Some(Outcome.succeeded[Id, Throwable, Int](7)))

    } yield ()

  }

}
