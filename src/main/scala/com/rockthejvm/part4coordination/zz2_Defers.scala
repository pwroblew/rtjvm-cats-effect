package com.rockthejvm.part4coordination

import cats.effect.*
import com.rockthejvm.utils.*

import scala.concurrent.duration.DurationInt

object zz2_Defers extends IOApp.Simple {

  val aDeferred: IO[Deferred[IO, Int]]    = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int]

  // get blocks
  val reader: IO[Int] = aDeferred.flatMap { signal =>
    signal.get // blocks the fiber
  }

  val writer: IO[Boolean] = aDeferred.flatMap { signal =>
    signal.complete(42)
  }

  // one time consumer/producer - was it in Scala Advanced course?
  def demoDeferred(): IO[Unit] = {

    def consumer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _     <- IO("[consumer] waiting for a results....").debug2
      value <- signal.get
      _     <- IO(s"[consumer] received value: $value").debug2
    } yield ()

    def producer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[producer]crunching numbers").debug2
      _ <- signal.complete(42)
      _ <- IO(s"[producer] submitted value: 42").debug2
    } yield ()

    for {
      signal <- IO.deferred[Int]
      fib1   <- consumer(signal).start
      fib2   <- producer(signal).start
      _      <- fib2.join
      _      <- fib1.join
    } yield ()
  }

  // downloading the file
  val fileParts                       = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")
  def fileNotifierWithRef(): IO[Unit] = {

    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts.map { part =>
        IO.sleep(1.second)
          >> IO(s"[receiver] received '$part'").debug2
          >> contentRef.update(_ + part)
      }.sequence.void

    def notifier(contentRef: Ref[IO, String]): IO[Unit] = {
      (
        IO("[notifier] ... downloading ...").debug2
          >> IO.sleep(500.millis)
      ).untilM_(contentRef.get.map(_.endsWith("<EOF>")))
        >> IO("[notifier] File download COMPLETE!!").debug2.void
    }

    for {
      downloadedContent <- IO.ref("")
      fib1              <- notifier(downloadedContent).start
      fib2              <- downloadFile(downloadedContent).start
      _                 <- fib1.join
      _                 <- fib2.join
    } yield ()

  }

  def fileNotifierWithRefAndDeferred(): IO[Unit] = {

    def downloadFile(contentRef: Ref[IO, String], signal: Deferred[IO, Boolean]): IO[Unit] =
      fileParts.map { part =>
        IO.sleep(1.second)
          >> IO(s"[receiver] received '$part'").debug2
          >> contentRef.update(_ + part)
          >> (if (part.endsWith("<EOF>")) signal.complete(true).void else IO.unit)
      }.sequence.void

    def notifier(contentRef: Ref[IO, String], signal: Deferred[IO, Boolean]): IO[Unit] = {
      signal.get
        >> IO("[notifier] File download COMPLETE!!").debug2.void
    }

    for {
      downloadedContent <- IO.ref("")
      signal            <- IO.deferred[Boolean]
      fib1              <- notifier(downloadedContent, signal).start
      fib2              <- downloadFile(downloadedContent, signal).start
      _                 <- fib1.join
      _                 <- fib2.join
    } yield ()

  }

  // exercise 1

  def alarmNotification() = {

    def countIncrementer(counter: Ref[IO, Int], signal: Deferred[IO, Boolean]): IO[Unit] = {
      val incrementAction = for {
        _     <- IO.sleep(1.second)
        count <- counter.updateAndGet(_ + 1)
        _     <- IO(s"[incrementer] ... incrementing... now it is $count").debug2
        _     <- if (count >= 10) signal.complete(true).void else IO.unit
      } yield ()
      incrementAction.untilM_(counter.get.map(_ > 12))
    }

    def nofifier(counter: Ref[IO, Int], signal: Deferred[IO, Boolean]): IO[Unit] = for {
      _ <- signal.get
      _ <- IO("time's up!").debug2
    } yield ()

    for {
      counter     <- IO.ref(0)
      signal      <- IO.deferred[Boolean]
      fibCounter  <- countIncrementer(counter, signal).start
      fibNotifier <- nofifier(counter, signal).start
      _           <- fibCounter.join
      _           <- fibNotifier.join
    } yield ()
  }

  def ourRacePair[A, B](
      ioa: IO[A],
      iob: IO[B]
  ): IO[Either[(OutcomeIO[A], FiberIO[B]), (FiberIO[A], OutcomeIO[B])]] = IO.uncancelable { poll =>
    for {
      signal <- IO.deferred[Either[OutcomeIO[A], OutcomeIO[B]]]
      fibA   <- ioa.guaranteeCase { outA => signal.complete(Left(outA)).void }.start
      fibB   <- iob.guaranteeCase { outB => signal.complete(Right(outB)).void }.start
      msg    <- poll(signal.get).onCancel {
                  for {
                    cancelA <- fibA.cancel.start
                    cancelB <- fibB.cancel.start
                    _       <- cancelA.join
                    _       <- cancelB.join
                  } yield ()
                }
    } yield msg match {
      case Left(outA)  => Left((outA, fibB))
      case Right(outB) => Right((fibA, outB))
    }
  }

  override def run: IO[Unit] =
    IO.unit
      // >> demoDeferred()
      // >> fileNotifierWithRef()
      // >> fileNotifierWithRefAndDeferred()
      >> alarmNotification()
}
