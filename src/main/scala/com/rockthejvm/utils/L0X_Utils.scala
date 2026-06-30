package com.rockthejvm.utils

import cats.effect.IO

extension [A](ioa: IO[A])
  def debug2: IO[A] = for {
    a <- ioa
    t  = Thread.currentThread().getName
    // _ <- IO.println(s"[$t]")
    _  = println(s"[$t] $a")
  } yield a
