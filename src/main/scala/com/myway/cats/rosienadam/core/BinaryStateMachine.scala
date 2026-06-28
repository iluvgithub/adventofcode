package com.myway.cats.rosienadam.core

import cats.effect._

import scala.concurrent.duration.DurationInt

trait BinaryStateMachine {
  def sleep: IO[Unit]
  def wakeUp: IO[Unit]
}

object BinaryStateMachine {
  sealed trait State
  case class Asleep(wakeUp: Deferred[IO, Unit]) extends State
  case object Awake                             extends State

  def asleep: IO[BinaryStateMachine] = for {
    wakeUp0     <- Deferred[IO, Unit]
    refIsAsleep <- Ref[IO].of[State](Asleep(wakeUp0))
    machine = new BinaryStateMachine {
      override def sleep: IO[Unit] = for {
        asleep <- Deferred[IO, Unit].map(Asleep(_))
        io <- refIsAsleep.modify {
          case z @ Asleep(wake) => (z, wake.get)
          case Awake            => (asleep, IO(()))
        }
        _ <- io
      } yield ()

      override def wakeUp: IO[Unit] = for {
        io <- refIsAsleep.modify {
          case Asleep(wake) => (Awake, wake.complete(()).void)
          case Awake        => (Awake, IO(()))
        }
        _ <- io
      } yield ()
    }
  } yield machine
}

object BinaryStateMachineMain extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for {
    machine <- BinaryStateMachine.asleep
    refOut  <- Ref[IO].of[Long](-41L)
    _       <- (machine.sleep *> refOut.set(42L), IO.sleep(200.millis) >> machine.wakeUp).parTupled.timeout(2.seconds)
    _ <- IO.sleep(200.millis)
    l       <- refOut.get
    _       <- IO(println(s"out=$l"))
  } yield ExitCode.Success
}
