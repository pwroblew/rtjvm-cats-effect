package com.rockthejvm.part4coordination

import cats.effect.std.CyclicBarrier
import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.implicits.catsSyntaxParallelTraverse1
import com.rockthejvm.utils.*

import scala.concurrent.duration.DurationInt
import scala.util.Random

object zz6_CyclicBarriers extends IOApp.Simple {

  def createUser(id: Int, barrier: MyCyclicBarrier): IO[Unit] = for {
    _ <- IO.sleep(Random.nextInt(500).millis)
    _ <- IO(s"[user $id] Just hear there is a new social network. Signing up for a waitilis").debug2
    _ <- IO.sleep(Random.nextInt(900).millis)
    _ <- IO(s"[user $id] User on the waitlist now!").debug2
    _ <- barrier.await
    _ <- IO(s"[user $id] OMG it is so cool!!").debug2
  } yield ()

  def openNetwork(): IO[Unit] = for {
    _       <- IO(
                 "[announcer] The RTJVM social network is up for registration! Launching when we have 10 users!"
               ).debug2
    barrier <- MyCyclicBarrier(10)
    _       <- (1 to 22).toList.parTraverse(id => createUser(id, barrier))
  } yield ()

  // exercise - implement cb

  case class CBState(num: Int, signal: Deferred[IO, Unit])

  class MyCyclicBarrier(n: Int, ref: Ref[IO, CBState]) {

    def await: IO[Unit] = for {
      newSignal <- IO.deferred[Unit]
      _         <- ref.modify(state =>
                     if (state.num <= 1) CBState(n, newSignal) -> state.signal.complete(()).void
                     else state.copy(num = state.num - 1)      -> state.signal.get
                   ).flatten
    } yield ()
  }

  object MyCyclicBarrier {
    def apply(n: Int): IO[MyCyclicBarrier] = for {
      signal <- IO.deferred[Unit]
      ref    <- IO.ref(CBState(n, signal))
    } yield new MyCyclicBarrier(n, ref)
  }

  override def run: IO[Unit] =
    IO.unit
      >> openNetwork()
}
