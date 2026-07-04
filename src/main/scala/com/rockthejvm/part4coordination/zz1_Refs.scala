package com.rockthejvm.part4coordination

import cats.effect.IO.catsSyntaxTuple2Parallel
import cats.effect.kernel.Ref
import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import scala.concurrent.duration.DurationInt

object zz1_Refs extends IOApp.Simple {

  val atomicMol: IO[Ref[IO, Int]]    = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  val increasedMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.set(42)
  }

  val mol: IO[Int] = atomicMol.flatMap { ref => ref.get }

  def demoConcurrentWork(lines: List[String]): IO[Int] = {
    for {
      initVal <- IO.ref(0)
      _       <- lines.map { line =>
                   val count: Int = line.split(" ").length
                   initVal.modify(oldCount =>
                     val newVal: Int = oldCount + count
                     (newVal, s"New value after adding $count is: $newVal")
                   ).debug2
                 }.parSequence
      res     <- initVal.get
    } yield res
  }

  val lines = List(
    "This is the very first line", // 6
    "some additional text",        // 3
    "learning CE is great",        // 4
    "but takes a lot of time"      // 6
  )                                // total: 19

  // exercise - refactor

  def tickingClockImpure(): IO[Unit] = {

    var ticks: Long = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug2
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug2
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()

    // actually there is no big issue with this implementation, as there is only 1 writer
    // hence racing is only between 1 writer and 1 reader. It means that the reader can
    // occasionally provide invalid data, however very rarely
    // Anyway, this is not thread-safe and not purely functional
  }

  def tickingClockPure(): IO[Unit] = {

    def clockTicking(ticks: Ref[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug2
      _ <- ticks.update(_ + 1)
    } yield ()

    def tickPrinting(ticks: Ref[IO, Int]): IO[Unit] = for {
      _        <- IO.sleep(5.seconds)
      ticksNum <- ticks.get
      _        <- IO(s"TICKS: $ticksNum").debug2
    } yield ()

    for {
      ticks <- Ref[IO].of[Int](0)
      _     <- (clockTicking(ticks).foreverM, tickPrinting(ticks).foreverM).parTupled
    } yield ()
  }

  override def run: IO[Unit] = IO.unit >>
    // demoConcurrentWork(lines).debug2 >>
    // tickingClockImpure() >>
    tickingClockPure() >>
    IO.unit
}
