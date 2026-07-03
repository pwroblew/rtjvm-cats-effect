package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import scala.concurrent.duration._

object CancellingIOs extends IOApp.Simple {

  import com.rockthejvm.utils._

  /*
    Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - manual cancellation
   */
  val chainOfIOs: IO[Int] = IO("waiting").debugD >> IO.canceled >> IO(42).debugD

  // uncancelable
  // example: online store, payment processor
  // payment process must NOT be canceled
  val specialPaymentSystem = (
    IO("Payment running, don't cancel me...").debugD >>
      IO.sleep(1.second) >>
      IO("Payment completed.").debugD
  ).onCancel(IO("MEGA CANCEL OF DOOM!").debugD.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _   <- IO.sleep(500.millis) >> fib.cancel
    _   <- fib.join
  } yield ()

  val atomicPayment    = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicPayment_v2 = specialPaymentSystem.uncancelable          // same

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _   <- IO.sleep(500.millis) >> IO("attempting cancellation...").debugD >> fib.cancel
    _   <- fib.join
  } yield ()

  /*
    The uncancelable API is more complex and more general.
    It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
    The Poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
   */

  /*
    Example: authentication service. Has two parts:
    - input password, can be cancelled, because otherwise we might block indefinitely on user input
    - verify password, CANNOT be cancelled once it's started
   */
  val inputPassword  = IO("Input password:").debugD >> IO("(typing password)").debugD >> IO.sleep(
    2.seconds
  ) >> IO("RockTheJVM1!")
  val verifyPassword =
    (pw: String) => IO("verifying...").debugD >> IO.sleep(2.seconds) >> IO(pw == "RockTheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw       <-
        poll(inputPassword).onCancel(
          IO("Authentication timed out. Try again later.").debugD.void
        ) // this is cancelable
      verified <- verifyPassword(pw)                                    // this is NOT cancelable
      _        <- if (verified) IO("Authentication successful.").debugD // this is NOT cancelable
                  else IO("Authentication failed.").debugD
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _       <- IO.sleep(3.seconds) >> IO(
                 "Authentication timeout, attempting cancel..."
               ).debugD >> authFib.cancel
    _       <- authFib.join
  } yield ()

  /*
    Uncancelable calls are MASKS which suppress cancellation.
    Poll calls are "gaps opened" in the uncancelable region.
   */

  /** Exercises: what do you think the following effects will do?
    *   1. Anticipate
    *   2. Run to see if you're correct
    *   3. Prove your theory
    */
  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).debugD
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debugD)
  // uncancelable will eliminate ALL cancel points

  // 2
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _       <- IO.sleep(1.seconds) >> IO(
                 "Authentication timeout, attempting cancel..."
               ).debugD >> authFib.cancel
    _       <- authFib.join
  } yield ()
  /*
    Lesson: Uncancelable calls are masks which suppress all existing cancelable gaps (including from a previous uncancelable).
   */

  // 3
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debugD >> IO.sleep(1.second) >> IO("cancelable end").debugD) >>
        IO("uncancelable").debugD >> IO.sleep(1.second) >> IO("uncancelable end").debugD >>
        poll(
          IO("second cancelable").debugD >> IO.sleep(1.second) >> IO("second cancelable end").debugD
        )
    }

    for {
      fib <- sequence.start
      _   <- IO.sleep(1500.millis) >> IO("CANCELING").debugD >> fib.cancel
      _   <- fib.join
    } yield ()
  }
  /*
    Lesson: Uncancelable regions ignore cancellation signals, but that doesn't mean the next CANCELABLE region won't take them.
   */

  override def run = threeStepProgram()
}
