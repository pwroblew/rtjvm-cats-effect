package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object L02_Resources extends IOApp.Simple {

  class Connection(url: String) {
    def open: IO[String]  = IO(s"opening connection to $url").debug2
    def close: IO[String] = IO(s"closing connection to $url").debug2
  }

  val asyncFetchUrl: IO[Unit] = for {
    fib1 <- (new Connection("rockthejvm.com")).open >> IO.sleep(Int.MaxValue.seconds).start
    _    <- IO.sleep(1.second) >> fib1.cancel
  } yield ()

  val correctAsyncFetchUrl: IO[Unit] = for {
    conn <- IO.pure(new Connection("blablabla.com"))
    fib1 <- (conn.open >> IO.sleep(Int.MaxValue.seconds))
              .onCancel(conn.close.void)
              .start
    _    <- IO.sleep(1.second) >> fib1.cancel
  } yield ()

  // bracket pattern
  private val bracketAsyncFetchUrl: IO[Unit] = IO.pure(new Connection("dfwef.com"))
    .bracket(conn => conn.open >> IO.sleep(Int.MaxValue.seconds))(_.close.void)

  val bracketProgram: IO[Unit] = for {
    fib1 <- bracketAsyncFetchUrl.start
    _    <- IO.sleep(500.millis) >> fib1.cancel
  } yield ()

  /*
  Exercise
   */

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def bracketReadFile(path: String): IO[String] =
    IO(s"Opening file at $path").debug2 >> openFileScanner(path)
      .bracket { scanner =>
        (IO.sleep(100.millis) >> IO.blocking(scanner.nextLine()).debug2).foreverM
      } { scanner =>
        IO(s"Closing file at $path").debug2 >> IO(scanner.close())
      }

  val bracketProgram_v2 = for {
    fib1 <- bracketReadFile("src/main/scala/com/rockthejvm/part3concurrency/Fibers.scala").start
    _    <- IO.sleep(1500.millis) // >> fib1.cancel
    _    <- fib1.join
  } yield ()

  override def run: IO[Unit] = bracketProgram_v2

}
