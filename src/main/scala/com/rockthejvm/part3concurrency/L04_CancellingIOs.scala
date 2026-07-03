package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.debug2

import scala.concurrent.duration.DurationInt

object L04_CancellingIOs extends IOApp.Simple {

  // manual cancellation

  val cancelledChainOfIOs =
    IO("waiting").debug2 >> IO.canceled >> IO(42).debug2 // the last one won't be evaluated

  // uncancellable

  val uncancellableChainOfIOs_v1 = cancelledChainOfIOs.uncancelable
  val uncancellableChainOfIOs_v2 = IO.uncancelable(_ => cancelledChainOfIOs)

  // payment system
  val specialPaymentSystemRaw =
    IO("Payment running. don't cancel me...").debug2 >>
      IO.sleep(1.second) >>
      IO("Payment completed.").debug2
  val specialPaymentSystem    =
    specialPaymentSystemRaw.onCancel(IO("MEGA CANCELL of DOOM!!!!").debug2.void)

  val specialPaymentSystemUncancellable_v1 =
    IO.uncancelable(_ => specialPaymentSystemRaw).onCancel(IO(
      "MEGA CANCELL of DOOM!!!!"
    ).debug2.void)

  val paymentCancel_v1 = for {
    fib1 <- specialPaymentSystem.start
    _    <- IO.sleep(500.millis) >> IO("Attempting cancellation").debug2 >> fib1.cancel
    _    <- fib1.join
  } yield ()

  val paymentCancelUncancellable_v1 = for {
    fib1 <- specialPaymentSystemUncancellable_v1.start
    _    <- IO.sleep(500.millis) >> IO("Attempting cancellation").debug2 >> fib1.cancel
    _    <- fib1.join
  } yield ()

  val specialPaymentSystemUncancellable_v2 =
    specialPaymentSystemRaw.uncancelable.onCancel(IO(
      "MEGA CANCELL of DOOM!!!!"
    ).debug2.void)
  val paymentCancelUncancellable_v2        = for {
    fib1 <- specialPaymentSystemUncancellable_v2.start
    _    <- IO.sleep(500.millis) >> IO("Attempting cancellation").debug2 >> fib1.cancel
    _    <- fib1.join
  } yield ()

  // understanding poll -> authentication service.
  val inputPassword: IO[String] =
    IO("Input password").debug2 >>
      IO("Typing password....").debug2 >>
      IO.sleep(3.seconds) >>
      IO("RockTheJVM!")

  val veriyfPassword: String => IO[Boolean] = (password: String) =>
    IO("verifying...").debug2 >> IO.sleep(3.seconds) >> IO(password == "RockTheJVM!").debug2

  val authFlowRaw: IO[Boolean] = for {
    pass <- inputPassword
    res  <- veriyfPassword(pass)
  } yield res

  val authFlow_v1 = IO.uncancelable { poll =>
    for {
      pass <- inputPassword.onCancel(IO("Authentication failed. Try again later.").debug2.void)
      res  <- veriyfPassword(pass)
      _    <- if (res) IO("Authentication successfull").debug2
              else IO("Authentication failed.").debug2
    } yield ()
  }

  val authProgram_v1 = for {
    authFib <- authFlow_v1.start
    _       <- IO.sleep(3.seconds) >> IO(
                 "Authentication timeouted, attempting cancel"
               ).debug2 >> authFib.cancel
    _       <- authFib.join
  } yield ()

  val authFlow_v2 = IO.uncancelable { poll =>
    for {
      pass <-
        poll(inputPassword).onCancel(IO("Authentication failed. Try again later.").debug2.void)
      res  <- veriyfPassword(pass)
      _    <- if (res) IO("Authentication successfull").debug2
              else IO("Authentication failed.").debug2
    } yield ()
  }

  val authProgram_v2 = for {
    authFib <- authFlow_v2.start
    _       <- IO.sleep(2.seconds) >>
                 IO("Authentication timeouted, attempting cancel").debug2 >>
                 authFib.cancel
    _       <- authFib.join
  } yield ()

  // exercises

  // 1.
  val cancelBeforeMol = IO.canceled >> IO(42).debug2
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debug2)

  // 2.
  val authProgram_v3 = for {
    authFib <- IO.uncancelable(_ => authFlow_v2).start
    _       <- IO.sleep(2.seconds) >>
                 IO("Authentication timeouted, attempting cancel").debug2 >>
                 authFib.cancel
    _       <- authFib.join
  } yield ()

  // 3

  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancellable1....").debug2 >> IO.sleep(1.second) >> IO("....cancellable1").debug2)
        .onCancel(IO("Cancelled1").debug2.void) >>
        IO("uncancellable....").debug2 >> IO.sleep(1.second) >> IO("....uncancellable").debug2
          .onCancel(IO("Cancelled2").debug2.void) >>
        poll(IO("cancellable3....").debug2 >> IO.sleep(1.second) >> IO("....cancellable3").debug2)
          .onCancel(IO("Cancelled3").debug2.void)
    }

    for {
      fib1 <- sequence.start
      _    <- IO.sleep(1500.millis) >> IO("Attempting cancellation").debug2 >> fib1.cancel
      _    <- fib1.join
    } yield ()
  }

  // X..

  val uncancellable3secs: IO[String] = IO.uncancelable { poll =>
    IO("3secs start").debug2 >>
      IO.sleep(1.second) >>
      poll(IO.sleep(1.second)) >>
      IO.sleep(1.second) >>
      IO("3secs end").debug2
  }

  val test1 = for {
    fib1 <- IO.uncancelable(_ => uncancellable3secs).start
    _    <- IO.sleep(1500.millis) >>
              IO("attempting cancellation").debug2 >>
              fib1.cancel >>
              IO("Cancelled").debug2
    _    <- fib1.join
  } yield ()

  override def run: IO[Unit] =
    IO.unit >>
      // cancelledChainOfIOs >>
      // uncancellableChainOfIOs_v1 >>
      // uncancellableChainOfIOs_v2 >>
      // paymentCancel_v1 >>
      // paymentCancelUncancellable_v1 >>
      // paymentCancelUncancellable_v2 >>
      // authFlowRaw >>
      // authFlow_v1 >>
      // authProgram_v1 >>
      // authProgram_v2 >>
      // cancelBeforeMol >>
      // uncancelableMol >>
      // authProgram_v3 >>
      // threeStepProgram() >>
      test1 >>
      IO.unit
}
