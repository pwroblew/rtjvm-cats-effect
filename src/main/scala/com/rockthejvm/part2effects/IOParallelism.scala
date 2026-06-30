package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {

  // IOs are usually sequential
  val aniIO    = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO = for {
    ani    <- aniIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  // debug extension method
  import com.rockthejvm.utils._
  // mapN extension method
  import cats.syntax.apply._
  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang: IO[String]    = IO.delay("Scala")
  val goalInLife             = (meaningOfLife.debugD, favLang.debugD).mapN((num, string) =>
    s"my goal in life is $num and $string"
  )

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int]                = Parallel[IO].parallel(meaningOfLife.debugD)
  val parIO2: IO.Par[String]             = Parallel[IO].parallel(favLang.debugD)
  import cats.effect.implicits._
  val goalInLifeParallel: IO.Par[String] =
    (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")
  // turn back to sequential
  val goalInLife_v2: IO[String]          = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand:
  import cats.syntax.parallel._
  val goalInLife_v3: IO[String] = (meaningOfLife.debugD, favLang.debugD).parMapN((num, string) =>
    s"my goal in life is $num and $string"
  )

  // regarding failure:
  val aFailure: IO[String]           = IO.raiseError(new RuntimeException("I can't do this!"))
  // compose success + failure
  val parallelWithFailure            =
    (meaningOfLife.debugD, aFailure.debugD).parMapN((num, string) => s"$num $string")
  // compose failure + failure
  val anotherFailure: IO[String]     = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String]        = (aFailure.debugD, anotherFailure.debugD).parMapN(_ + _)
  // the first effect to fail gives the failure of the result
  val twoFailuresDelayed: IO[String] =
    (IO(Thread.sleep(1000)) >> aFailure.debugD, anotherFailure.debugD).parMapN(_ + _)

  override def run: IO[Unit] =
    twoFailuresDelayed.debugD.void
}
