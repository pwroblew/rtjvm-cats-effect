package com.rockthejvm.part2effects

import cats.{Parallel, Traverse}
import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random
import cats.syntax.traverse.*

object L06_IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global
  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List(
    "I quite like CE",
    "Scala is great",
    "looking forward to some awesome stuff",
    "let's make a break"
  )

  private val eventualInts_1: List[Future[Int]] = workLoad.map(heavyComputation)

  private val eventualInts_2: Future[List[Int]] = workLoad.traverse(heavyComputation)

  // --------------

  import com.rockthejvm.utils._
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug2

  private val list: List[IO[Int]]     = workLoad.map(computeAsIO)
  private val singleIO: IO[List[Int]] = workLoad.traverse(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._
  val parallelSindleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /*
  Exercises:
  1. sequence
   */

  def sequence_v2[A](listOfIOs: List[IO[A]]): IO[List[A]]              =
    Traverse[List].traverse(listOfIOs)(identity)
  def sequence_v3[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]]    =
    Traverse[F].traverse(listOfIOs)(identity)
  def parSequence_v2[A](listOfIOs: List[IO[A]]): IO[List[A]]           =
    listOfIOs.parTraverse(identity)
  def parSequence_v3[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    listOfIOs.parTraverse(identity)

  override def run: IO[Unit] = parallelSindleIO.void
}
