package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp, Outcome, OutcomeIO}
import com.rockthejvm.utils.*

import scala.concurrent.duration.DurationInt

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

  // override def run: IO[Unit] = differentThreadIOs().debug2.void
  // override def run: IO[Unit] = runOnAnotherThread(meaningOfLife.debug2).debug2.void
  // override def run: IO[Unit] = throwOnAnotherThread().debug2.void
  override def run: IO[Unit] = testCancel().debug2.void
}
