package com.rockthejvm.part2effects

import cats.effect.{ExitCode, IO, IOApp}

object L04_IOApps {
  val program: IO[Unit] = for {
    line <- IO.readLine
    _    <- IO.println(s"You've just written: $line")
  } yield ()
}

object FirstCEApp_4 extends IOApp {
  import L04_IOApps._
  override def run(args: List[String]): IO[ExitCode] =
    program.as(ExitCode.Success)
}
