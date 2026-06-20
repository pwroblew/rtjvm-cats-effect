package com.rockthejvm.part1recap

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object L01_Essentials {

  val unit: Unit = ()

  def tryPlay = {
    val anAttempt1 = Try(42)
    anAttempt1 match {
      case Success(value) => println(s"Success1: $value")
      case Failure(exception) => println(s"Failure1: $exception")
    }

    val anAttempt2 = Try(1 / 0)
    anAttempt2 match {
      case Success(value) => println(s"Success2: $value")
      case Failure(exception) => println(s"Failure2: $exception")
    }
  }

  def futuresPlay = {
    val threadPool: ExecutorService = Executors.newFixedThreadPool(10)
    given ExecutionContext = ExecutionContext.fromExecutorService(threadPool)

    val aFuture1: Future[Int] = Future {
      1 / 0
    }

    aFuture1.onComplete {
      case Success(value) =>
        threadPool.shutdown()
        println(s"Success1: $value")
      case Failure(exception) =>
        threadPool.shutdown()
        println(s"Failure1: $exception")
    }

    aFuture1.failed.map(exception => println(s"AAA Failure1: $exception"))

  }


  trait SequenceChecker[A[_]] {
    def isSequential: Boolean
  }

  val listSequenceChecker: SequenceChecker[List] = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {
    tryPlay
    futuresPlay
  }


}
