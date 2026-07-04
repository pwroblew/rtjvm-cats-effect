package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.{IO, IOApp}
import cats.implicits.catsSyntaxOptionId
import com.rockthejvm.utils.*

import java.util
import java.util.concurrent.{Callable, ExecutorService, Executors}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success, Try}

object L06_AsyncIOs extends IOApp.Simple {

  type Callback[A] = Either[Throwable, A] => Unit

  val threadPool: ExecutorService         = Executors.newFixedThreadPool(6)
  val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(threadPool)

  def computeTheMeaningOfLifeRaw(): Int = {
    println(
      s"[${Thread.currentThread().getName}] starting computation of the MOL on some other thread...."
    )
    Thread.sleep(1000)
    println(
      s"[${Thread.currentThread().getName}] completed computation of the MOL on some other thread...."
    )
    42
  }

  def computeTheMeaningOfLife(): Either[Throwable, Int] = Try {
    computeTheMeaningOfLifeRaw()
  }.toEither

  def computeMolOnThreadPool(): Unit = // we can't use it, and get the result...
    threadPool.execute(() => computeTheMeaningOfLife())

  val asyncMol: IO[Int] = IO.async_ { cb =>
    val value: util.concurrent.Future[Unit] = threadPool.submit { () =>
      {
        val res: Either[Throwable, Int] = computeTheMeaningOfLife()
        cb(res)
      }
    }
  }

  // exercise

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = IO.async_ { cb =>
    ec.execute { () =>
      val result: Either[Throwable, A] = Try {
        computation()
      }.toEither
      cb(result)
    }
  }

  val asyncMol_v2: IO[Int] = asyncToIO(computeTheMeaningOfLifeRaw)(ec)

  // exercise 2

  lazy val molFuture: Future[Int]  = Future { computeTheMeaningOfLifeRaw() }(ec)
  lazy val molFuture2: Future[Int] = Future {
    println("Somewhere in another thread....")
    43
  }(ec)

  def future2IO[A](future: => Future[A]): IO[A] = IO.async_ { cb =>
    future.onComplete {
      case Success(value)     => cb(Right(value).withLeft[Throwable])
      case Failure(exception) => cb(Left(exception).withRight[A])
    }
  }

  // exercise 3 - never ending IO

  def neverEndingIO[A]: IO[A] = IO.async_ { cb => () }

  // FULL ASYNC

  val asyncMolFull: IO[Int] = IO.async { cb =>
    val javaFuture: util.concurrent.Future[Unit] = threadPool.submit { () =>
      {
        val res: Either[Throwable, Int] = computeTheMeaningOfLife()
        cb(res)
      }
    }
    // cancellation finalizer
    IO("Finalizer called after cancelling").debug2 >>
      IO {
        IO(javaFuture.cancel(true)).void.some
      }
  }

  def asyncFullDemo(): IO[Int] = {

    val fibResult: IO[Outcome[IO, Throwable, Int]] = for {
      fib <- asyncMolFull.start
      _   <- IO.sleep(500.millis) >> IO("Attempting cancellation").debug2 >> fib.cancel
      res <- fib.join
    } yield res

    fibResult.flatMap { outcome =>
      outcome.embed(IO(
        "Got cancelled"
      ).debug2 >> IO.raiseError[Int](new RuntimeException("cancellation happened")))
    }

  }

  override def run: IO[Unit] = IO.unit >>
    asyncMol_v2.debug2 >>
    future2IO(molFuture).debug2 >>
    IO.fromFuture(IO(molFuture2)).debug2 >>
    asyncFullDemo().debug2 >>
    IO.delay(threadPool.shutdown())
}
