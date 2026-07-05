package com.rockthejvm.part4coordination

import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.implicits.catsSyntaxParallelTraverse1

import scala.concurrent.duration.DurationInt
import scala.util.Random
import com.rockthejvm.utils.*

trait Mutex2 {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Mutex2 {

  case class State2(isAcquired: Boolean, signal: Deferred[IO, Unit])

  def create: IO[Mutex2] = IO.deferred[Unit].flatMap { firstSignal =>
    IO.ref(State2(false, firstSignal)).map { ref =>
      new Mutex2 {

        override def acquire: IO[Unit] = for {
          newSignal   <- IO.deferred[Unit]
          hasAcquired <- ref.modify { state =>
                           if (state.isAcquired) {
                             (state, false)
                           } else {
                             (State2(true, newSignal), true)
                           }
                         }

          _ <- if (hasAcquired) IO.unit
               else for {
                 state <- ref.get
                 _     <- state.signal.get // blocking
                 _     <- acquire
               } yield ()

        } yield ()

        override def release: IO[Unit] = for {
          newState <- ref.getAndUpdate { state =>
                        if (!state.isAcquired) state
                        else State2(false, state.signal)
                      }
          _        <- newState.signal.complete(())

        } yield ()
      }
    }
  }

}

object zz3_Mutex extends IOApp.Simple {

  def criticalTask(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def createNonLockingTask(id: Int): IO[Int] = for {
    _   <- IO(s"[task $id] - starting the work").debug2
    res <- criticalTask()
    _   <- IO(s"[task $id] - work completed, result: $res").debug2
  } yield res

  def demoNonLockingTasks(): IO[List[Int]] = (1 to 10).toList.parTraverse(createNonLockingTask)

  def createLockingTask(id: Int, mutex: Mutex2): IO[Int] = for {
    _   <- IO(s"[task $id] - waiting for permission to start").debug2
    _   <- mutex.acquire
    _   <- IO(s"[task $id] - starting the work").debug2
    res <- criticalTask()
    _   <- IO(s"[task $id] - work completed, result: $res").debug2
    _   <- mutex.release
    _   <- IO(s"[task $id] - mutex released").debug2
  } yield res

  def demoLockingTasks(): IO[List[Int]] = for {
    mutex  <- Mutex2.create
    result <- (1 to 10).toList.parTraverse(createLockingTask(_, mutex))
  } yield result

  def demoCancelWithBlocked() = {
    for {
      mutex <- Mutex2.create
      fib1  <- (
                 IO("[fib1] getting mutex").debug2
                   >> mutex.acquire
                   >> IO("[fib1] got the mutex, never releasing").debug2
                   >> IO.never
               ).start
      fib2  <- (
                 IO("[fib2] sleeping").debug2
                   >> IO.sleep(1.second)
                   >> IO("[fib2] trying to get the mutex").debug2
                   >> mutex.acquire
                   >> IO("[fib2] mutex acquired").debug2
               ).start
      fib3  <- (
                 IO("[fib3] sleeping").debug2
                   >> IO.sleep(1500.millis)
                   >> IO("[fib3] trying to get the mutex").debug2
                   >> mutex.acquire
                   >> IO("[fib3] mutex acquired. If this shows then FAIL!").debug2
               ).start
      _     <- IO.sleep(2.seconds) >> IO("Cancelling job 2").debug2 >> fib2.cancel
      _     <- fib1.join
      _     <- fib2.join
      _     <- fib3.join
    } yield ()
  } // ha, my impoementation works fine!!

  override def run: IO[Unit] =
    IO.unit
      // >> demoNonLockingTasks()
      // >> demoLockingTasks()
      >> demoCancelWithBlocked()
      >> IO.unit
}
