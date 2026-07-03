package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import cats.implicits.toTraverseOps
import com.rockthejvm.utils.debug2

import scala.concurrent.duration.DurationInt
import scala.util.Random

object L05_BlockingIOs extends IOApp.Simple {

  val someSleeps = for {
    _ <- IO.sleep(1.second).debug2
    _ <- IO.cede
    _ <- IO.sleep(1.second).debug2
  } yield ()

  val moreFibers = for {
    fibers <- (1 to 10000).map(n =>
                (
                  IO.sleep(Random.between(500, 1500).millis) >>
                    IO(n).debug2 >>
                    IO.sleep(Random.between(500, 1500).millis) >>
                    IO(n).debug2 >>
                    IO.sleep(Random.between(500, 1500).millis) >>
                    IO(n).debug2
                ).start
              ).toList.sequence
    _      <- fibers.traverse(_.join)
  } yield ()

  override def run: IO[Unit] = IO.unit >>
    moreFibers
}
