package com.rockthejvm.part4coordination

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import cats.implicits.catsSyntaxParallelTraverse1

import scala.concurrent.duration.DurationInt
import scala.util.Random
import com.rockthejvm.utils.*

object zz4_Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2)

  // example: limiting the number of concurrent sessions on a server

  def doStuffWhileLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def loginAndDoStuff(id: Int, semaphore: Semaphore[IO]): IO[Int] = for {
    _   <- IO(s"[$id] attempting to log in").debug2
    _   <- semaphore.acquire
    _   <- IO(s"[$id] logged in, starting the work").debug2
    res <- doStuffWhileLoggedIn()
    _   <- IO(s"[$id] work done: $res, logging out").debug2
    _   <- semaphore.release
    _   <- IO(s"[$id] bye").debug2
  } yield res

  def demoLogin(): IO[List[Int]] = for {
    semaphore  <- Semaphore[IO](2)
    outputList <- List(1, 2, 3).parTraverse(id =>
                    loginAndDoStuff(id, semaphore)
                  )
    _          <- IO(outputList).debug2
  } yield outputList

  def weightedLoginAndDoStuff(id: Int, permitsRequested: Int, semaphore: Semaphore[IO]): IO[Int] =
    for {
      _   <- IO(s"[$id] attempting to log in").debug2
      _   <- semaphore.acquireN(permitsRequested)
      _   <- IO(s"[$id] logged in, starting the work").debug2
      res <- doStuffWhileLoggedIn()
      _   <- IO(s"[$id] work done: $res, logging out").debug2
      _   <- semaphore.releaseN(permitsRequested)
      _   <- IO(s"[$id] bye").debug2
    } yield res

  def demoWeightedLogin(): IO[List[Int]] = for {
    semaphore  <- Semaphore[IO](2)
    outputList <- List(1, 2, 3).parTraverse(id =>
                    weightedLoginAndDoStuff(id, id, semaphore)
                  )
    _          <- IO(outputList).debug2
  } yield outputList

  override def run: IO[Unit] =
    IO.unit
      // >> demoLogin()
      >> demoWeightedLogin()
      >> IO.unit
}
