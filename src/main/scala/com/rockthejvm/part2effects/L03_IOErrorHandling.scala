package com.rockthejvm.part2effects

import cats.effect.IO

import scala.util.Try

object L03_IOErrorHandling {

  val aFailedIO: IO[Int]   = IO.delay(throw new RuntimeException("A FAILURE"))
  val aFailedIO_2: IO[Int] = IO.raiseError(new RuntimeException("Yet another failure"))

  // handling exceptions
  val dealWithIt: IO[AnyVal] = aFailedIO_2.handleErrorWith {
    case _: RuntimeException => IO.println("Im still here")
  }

  private val attempt: IO[Either[Throwable, Int]] = aFailedIO_2.attempt

  private val processedFailure: IO[Unit] = aFailedIO_2.redeemWith(
    th => IO.println(s"Failed: ${th.getMessage}"),
    value => IO.println(s"Success: $value")
  )

  /** Excersises:
    */

  // 1. construct potentially failed IOs from standard data types
  def option2IO[A](option: Option[A])(ifEmpty: => Throwable): IO[A] =
    option.fold(IO.raiseError(ifEmpty))(IO.pure)

  def try2IO[A](aTry: Try[A]): IO[A] =
    aTry.fold(IO.raiseError, IO.pure)

  def either2IO[A](either: Either[Throwable, A]): IO[A] =
    either.fold(IO.raiseError, IO.pure)

  // 2. handleError, handleErrorWith
  def handleError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.attempt.flatMap {
    case Left(e)      => IO.delay(handler(e))
    case Right(value) => IO.pure(value)
  }

  def handleErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.attempt.flatMap {
    case Left(e)      => handler(e)
    case Right(value) => IO.pure(value)
  }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    // aFailedIO.unsafeRunSync()
    // aFailedIO_2.unsafeRunSync()
    // dealWithIt.unsafeRunSync()
    processedFailure.unsafeRunSync()

  }
}
