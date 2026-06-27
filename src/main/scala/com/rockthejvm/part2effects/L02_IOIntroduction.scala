package com.rockthejvm.part2effects

import cats.effect.IO

import scala.annotation.tailrec

object L02_IOIntroduction {

  val firstIO: IO[Int]         = IO.pure(42)
  val delayedIO: IO[Int]       = IO.delay {
    println("I'm producing an integer")
    54
  }
  val shouldNotDoThis: IO[Int] = IO.pure {
    println("I'm producing an integer, but WRONG")
    54
  }

  val aDelayedIO2: IO[Int] = IO.apply {
    println("I'm producing an integer")
    54
  }

  val smallProgram: IO[Unit] = for {
    line1 <- IO.readLine
    line2 <- IO.readLine
    _     <- IO.println(line1 + line2)
  } yield ()

  /** exercises
    *   1. sequence two IOs and take the result of the last
    *   2. sequence two IOs and take the result of the first
    *   3. repeat and IO effect forever
    *   4. convert an IO to other type
    *   5. discard the value of IO and return Unit
    */

  def sequenceTakeLast_v1[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    _ <- ioa
    b <- iob
  } yield b

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(a => iob.map(b => b))

  def sequenceTakeFirst_v1[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    a <- ioa
    _ <- iob
  } yield a

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(b => a))

//  def forever_v1[A](ioa: IO[A]): IO[A]             =
//    sequenceTakeLast_v1(ioa, forever_v1(ioa))

  def forever_v2[A](ioa: IO[A]): IO[A] =
    ioa.flatMap(a => forever_v2(ioa))

  def convert[A, B](io: IO[A], value: B): IO[B] = io.map(a => value)

  def asUnit[A](ioa: IO[A]): IO[Unit] = convert(ioa, ())

  def sumN_v1(n: Int): Int =
    if n <= 0 then 0
    else n + sumN_v1(n - 1)

  def sumN_v2(n: Int): Int = {
    @tailrec
    def loop(n: Int, agg: Int): Int = {
      if n <= 0 then agg
      else loop(n - 1, agg + n)
    }
    loop(n, 0)
  }

  def sumNIO_v1(n: Int): IO[Int] =
    if n <= 0 then IO.pure(0)
    else IO.pure(n).flatMap(n => sumNIO_v1(n - 1).map(_ + n))

  def fib_v1(n: Int): BigInt =
    if n <= 1 then 1
    else fib_v1(n - 1) + fib_v1(n - 2)

  def fib_v2(n: Int): BigInt = {
    @tailrec
    def loop(k: Int, agg1: BigInt, agg2: BigInt): BigInt = {
      if k >= n then agg1
      else loop(k + 1, agg1 + agg2, agg1)
    }
    loop(1, 1, 1)
  }

  /*
  n = 6

  k=1: loop(1, 1, 1)
  k=2: loop(2, 1+1, 1)
  k=3: loop(3, 2+1, 2)
  k=4: loop(4, 3+2, 3)

   */

  def fibIO_v1(n: Int): IO[BigInt] =
    if n <= 1 then IO.pure(1)
    else fibIO_v1(n - 1).flatMap(larger => fibIO_v1(n - 2).map(smaller => smaller + larger))

  def fibIO_v11(n: Int): IO[BigInt] =
    if n <= 1 then IO.pure(1)
    else
      for {
        larger  <- fibIO_v11(n - 1)
        smaller <- fibIO_v11(n - 2)
      } yield larger + smaller

  def fibIO_v2(n: Int): IO[BigInt] = {
    @tailrec
    def loop(k: Int, agg1: BigInt, agg2: BigInt): IO[BigInt] = {
      if k >= n then IO.pure(agg1)
      else loop(k + 1, agg1 + agg2, agg1)
    }

    loop(1, 1, 1)
  }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits._
//    println(delayedIO.unsafeRunSync())
//    smallProgram.unsafeRunSync()

    val foreverIO: IO[Unit] = forever_v2(IO.println("aa"))
    // foreverIO.unsafeRunSync() // works as expected

    println(sumN_v2(1000000))
    println(sumNIO_v1(1000000).unsafeRunSync())
    println(fib_v2(1000))
    println(fibIO_v2(1000).unsafeRunSync())
    println(fibIO_v11(10).unsafeRunSync())

  }
}
