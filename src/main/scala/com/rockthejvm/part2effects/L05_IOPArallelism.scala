package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.IO.Par
import cats.effect.{IO, IOApp}
import cats.implicits.catsSyntaxTuple2Semigroupal
import com.rockthejvm.utils.*

object L05_IOPArallelism extends IOApp.Simple {
  val anisIO: IO[String]   = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO: IO[String] = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO: IO[String] = for {
    ani    <- anisIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love RockTheJVM"

  val meaningOfLife = IO.delay(42)
  val favLang       = IO.delay("Scala")

  val goalInLife = (meaningOfLife.debug2, favLang.debug2).mapN { (num, string) =>
    s"my goal in life is $num and $string"
  }

  // parallelism on IOs
  // convert sequential IO into a parallel IO

  val parIO_1: IO.Par[Int]          = Parallel[IO].parallel(meaningOfLife.debug2)
  val parIO_2: IO.Par[String]       = Parallel[IO].parallel(favLang.debug2)
  val goalInLifePar: IO.Par[String] = (parIO_1, parIO_2).mapN { (num, string) =>
    s"my goal in life is $num and $string"
  }
  val goalInLife_2: IO[String]      = Parallel[IO].sequential(goalInLifePar)

  val goalInLife_3: IO[String] = (meaningOfLife.debug2, favLang.debug2).parMapN { (num, string) =>
    s"my goal in life is $num and $string"
  }

  // failures
  val aStringFailure1: IO[String] = IO.raiseError(new RuntimeException("Failure String"))
  val anIntFailure1: IO[Int]      = IO.raiseError(new RuntimeException("Failure Int"))

  val goalWithFailure: IO[String] = (meaningOfLife.debug2, aStringFailure1.debug2).parMapN {
    (num, string) =>
      s"my goal in life is $num and $string"
  }

  val twoFailures_1: IO[String] = (anIntFailure1.debug2, aStringFailure1.debug2).parMapN {
    (num, string) => s"my goal in life is $num and $string"
  }

  val twoFailures_2: IO[String] =
    (anIntFailure1.debug2, IO(Thread.sleep(1000)) >> aStringFailure1.debug2).parMapN {
      (num, string) => s"my goal in life is $num and $string"
    }

  // override def run: IO[Unit] = composedIO.map(println)
  // override def run: IO[Unit] = goalInLife.map(println)
  // override def run: IO[Unit] = goalInLife_2.debug2.void
  // override def run: IO[Unit] = goalInLife_3.debug2.void
  // override def run: IO[Unit] = goalWithFailure.debug2.void
  // override def run: IO[Unit] = twoFailures_1.debug2.void
  override def run: IO[Unit] =
    twoFailures_2
      .handleErrorWith(th => IO.println(s"Error: ${th.getMessage}"))
      .debug2.void
}
