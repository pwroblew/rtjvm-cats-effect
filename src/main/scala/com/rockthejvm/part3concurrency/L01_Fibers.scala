package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp, Outcome, OutcomeIO}
import com.rockthejvm.utils.*

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object L01_Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang       = IO.pure("Scala")

  def sameThreadIOs(): IO[Unit] = for {
    _ <- meaningOfLife.debug2
    _ <- favLang.debug2
  } yield ()

  // introducing fibers
  def createFiber: Fiber[IO, Throwable, String] = ???

  private val startFiber: IO[FiberIO[Int]]               = meaningOfLife.debug2.start
  private val startFiber2: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug2.start

  def differentThreadIOs(): IO[Unit] = for {
    _ <- meaningOfLife.debug2.start
    _ <- favLang.debug2.start
  } yield ()

  // joining the fiber
  def runOnAnotherThread[A](ioa: IO[A]): IO[OutcomeIO[A]] = for {
    fib    <- ioa.start
    result <- fib.join
  } yield result

  val someIOOnAnotherThread                        = runOnAnotherThread(meaningOfLife.debug2)
  private val someResultFromAnotherThread: IO[Int] = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e)        => IO(0)
    case Canceled()        => IO(-1)
  }

  def throwOnAnotherThread() = for {
    fib    <- IO.raiseError[Int](new RuntimeException("INT failure")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task                        = IO("starting").debug2 >> IO.sleep(1.second) >> IO("done")
    val taskWithCancellationHandler = task.onCancel(IO("Im being cancelled now").debug2.void)

    for {
      fib    <- taskWithCancellationHandler.start
      _      <- IO.sleep(500.millis) >> IO("cancelling").debug2 >> fib.cancel
      result <- fib.join
    } yield result
  }

  /*
  Exercises:
  1. a function that runs IO on another thread and returns IO or failed IO.
  2. a function taking 2 IOs and running them concurrently in 2 threads and returning a pair.
   */

  def processResultFromFiber[A](ioa: IO[A]): IO[A] = for {
    fib1    <- ioa.debug2.start
    // _       <- IO.sleep(500.millis) >> IO("cancelling").debug2 >> fib1.cancel
    result  <- fib1.join
    outcome <- result match {
                 case Succeeded(effect) => effect
                 case Errored(e)        => IO.raiseError[A](e)
                 case Canceled()        =>
                   IO.raiseError[A](new RuntimeException("The calculation got cancelled."))
               }
  } yield outcome

  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = for {
    fibA    <- ioa.start
    fibB    <- iob.start
    res1    <- fibA.join
    res2    <- fibB.join
    outcome <- (res1, res2) match {
                 case (Canceled(), _) | (_, Canceled())    =>
                   IO.raiseError[(A, B)](new RuntimeException("One of computations got cancelled."))
                 case (Errored(e), _)                      => IO.raiseError[(A, B)](e)
                 case (_, Errored(e))                      => IO.raiseError[(A, B)](e)
                 case (Succeeded(eff_A), Succeeded(eff_B)) => for {
                     a <- eff_A
                     b <- eff_B
                   } yield (a, b)
               }
  } yield outcome

  def timeout[A](ioa: IO[A], duration: FiniteDuration): IO[A] = for {
    fib_A    <- ioa.start
    fib_X    <- (IO.sleep(duration) >> fib_A.cancel).start
    result_A <- fib_A.join
    _        <- fib_X.cancel
    outcome  <- result_A match {
                  case Canceled()        =>
                    IO.raiseError[A](new RuntimeException("The computation got cancelled"))
                  case Errored(e)        => IO.raiseError[A](e)
                  case Succeeded(effect) => effect
                }
  } yield outcome

  // override def run: IO[Unit] = differentThreadIOs().debug2.void
  // override def run: IO[Unit] = runOnAnotherThread(meaningOfLife.debug2).debug2.void
  // override def run: IO[Unit] = throwOnAnotherThread().debug2.void
  // override def run: IO[Unit] = testCancel().debug2.void
  // override def run: IO[Unit] = processResultFromFiber(IO.sleep(1.second) >> IO.pure(42)).debug2.void

  /*
  override def run: IO[Unit] = tupleIOs(
    IO.sleep(200.millis) >> IO("first").debug2,
    IO.sleep(300.millis) >> IO(
      "second"
    ).debug2 >> IO.raiseError[String](new RuntimeException("ERROR!!!$"))
  ).debug2.void

   */

  override def run: IO[Unit] =
    timeout(IO.sleep(5.seconds) >> IO("42").debug2, 2.seconds).debug2.void
}
