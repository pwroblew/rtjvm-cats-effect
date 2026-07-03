package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{FiberIO, IO, IOApp, OutcomeIO}
import com.rockthejvm.utils.*

import scala.concurrent.TimeoutException
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object L03_RacingIOs extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] = {
    val iosChain: IO[A] = IO(s"computation for $value: starting").debug2 >>
      IO.sleep(duration) >>
      IO(s"computation for $value: done").debug2 >>
      IO(value)
    iosChain.onCancel(IO(s"computation for $value: CANCELLED!").debug2.void)
  }

  def testRace(): IO[Unit] = {
    val meaningOfLife: IO[Int]          = runWithSleep(42, 1.second)
    val favLang: IO[String]             = runWithSleep("Scala", 2.second)
    val result: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    val value: IO[String]               = result.flatMap {
      case Left(num)     => IO(s"The meaning of life $num won!").debug2
      case Right(string) => IO(s"Fav lang $string won!").debug2
    }
    value.void
  }

  def testRacePair(): IO[Unit] = {
    val meaningOfLife: IO[Int]                                                                   = runWithSleep(42, 1.second)
    val favLang: IO[String]                                                                      = runWithSleep("Scala", 2.second)
    val result: IO[Either[(OutcomeIO[Int], FiberIO[String]), (FiberIO[Int], OutcomeIO[String])]] =
      IO.racePair(meaningOfLife, favLang)
    result.flatMap {
      case Left((outMol, fibLang))  =>
        fibLang.cancel >> IO(s"The meaning of life won: $outMol!").debug2 >> IO(outMol).debug2
      case Right((fibMol, outLang)) =>
        fibMol.cancel >> IO(s"Fav lang won: $outLang!").debug2 >> IO(outLang).debug2
    }.void
  }

  // Exercises

  // implement the timeout pattern with race

  def timeout[A](ioa: IO[A], duration: FiniteDuration): IO[A] = {
    IO.race(ioa, IO.sleep(duration)).flatMap {
      case Left(a)  => IO(a)
      case Right(_) => IO.raiseError[A](new TimeoutException("IO cancelled because of timeout"))
    }
  }

  // returning loosing effect
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB))  =>
        fibB.join.flatMap(reduceOutcome).map(Right(_).withLeft[A])
      case Right((fibA, _)) =>
        fibA.join.flatMap(reduceOutcome).map(Left(_).withRight[B])
    }
  }

  def reduceOutcome[A](outcomeIO: OutcomeIO[A]): IO[A] = {
    outcomeIO match {
      case Canceled()        =>
        IO.raiseError[A](new RuntimeException("The computation got cancelled"))
      case Errored(e)        => IO.raiseError[A](e)
      case Succeeded(effect) => effect
    }
  }

  // simple race
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB))  => for {
          _ <- fibB.cancel
          a <- reduceOutcome(outA)
        } yield Left(a).withRight[B]
      case Right((fibA, outB)) => for {
          _ <- fibA.cancel
          b <- reduceOutcome(outB)
        } yield Right(b).withLeft[A]
    }

  }

  def testRaceSimple(): IO[Unit] = {
    val meaningOfLife: IO[Int]          = runWithSleep(42, 1.second)
    val favLang: IO[String]             = runWithSleep("Scala", 2.second)
    val result: IO[Either[Int, String]] = simpleRace(meaningOfLife, favLang)
    val value: IO[String]               = result.flatMap {
      case Left(num)     => IO(s"The meaning of life $num won!").debug2
      case Right(string) => IO(s"Fav lang $string won!").debug2
    }
    value.void
  }

  override def run: IO[Unit] = {
    // testRacePair().debug2

    val io1sec: IO[Int] = IO.sleep(1.second) >> IO(42)

    // timeout(io1sec, 1500.millis).debug2 >>
    // timeout(io1sec, 500.millis).debug2 >>
    // unrace(io1sec, IO.sleep(500.millis)).debug2 >>
    testRaceSimple() >>
      IO.unit

  }
}
