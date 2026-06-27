package com.rockthejvm.part2effects

object L01_Effects {

  def main(args: Array[String]): Unit = {

    case class MyIO[A](unsafeRun: () => A) {
      def map[B](f: A => B): MyIO[B]           = MyIO { () => f(unsafeRun()) }
      def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO { () => f(unsafeRun()).unsafeRun() }
    }

    val anIO_1 = MyIO { () =>
      println("I'm writing something....")
      42
    }

    /** Excersises:
      *   1. build an IO which return the current time of the system (at the moment of calling
      *      unsafeRun)
      *   2. An IO which meaures the duration of computation
      *   3. An IO that prints something to the console
      *   4. An Io that reads smth from the console
      */

    val systemTimeIO: MyIO[Long] = MyIO(() => System.currentTimeMillis())

    def measureTime[A](computation: MyIO[A]): MyIO[Long] = {
      systemTimeIO.flatMap(start =>
        computation.flatMap(result => systemTimeIO.map(end => end - start))
      )
    }

    // ---------------
    val computationX: MyIO[Int] = MyIO[Int] { () => 42 }
    systemTimeIO.flatMap(start =>
      computationX.flatMap(result => systemTimeIO.map(end => end - start))
    )
    systemTimeIO.flatMap(start =>
      computationX.flatMap(result => MyIO(() => System.currentTimeMillis()).map(end => end - start))
    )
    systemTimeIO.flatMap(start =>
      computationX.flatMap(result =>
        MyIO { () =>
          {
            val end: Long = System.currentTimeMillis()
            end - start
          }
        }
      )
    )
    systemTimeIO.flatMap(start =>
      MyIO[Int] { () => 42 }.flatMap(result =>
        MyIO { () =>
          {
            val end: Long = System.currentTimeMillis()
            end - start
          }
        }
      )
    )
    systemTimeIO.flatMap(start =>
      MyIO { () =>
        {
          val res = 42
          val end: Long = System.currentTimeMillis()
          end - start
        }
      }
    )
    MyIO(() => System.currentTimeMillis()).flatMap(start =>
      MyIO { () =>
        {
          val res = 42
          val end: Long = System.currentTimeMillis()
          end - start
        }
      }
    )
    val res = MyIO { () =>
      val start: Long = System.currentTimeMillis()
      val res   = 42
      val end: Long = System.currentTimeMillis()
      end - start
    }

    // ---------------
    def measureTime2[A](computation: MyIO[A]): MyIO[Long] = for {
      startTime <- systemTimeIO
      _         <- computation
      endTime   <- systemTimeIO
    } yield endTime - startTime

    val anIO_2 = MyIO { () =>
      println("Starting doing something....")
      Thread.sleep(1000)
      println(" done with the stuff after 1 second")
    }

    val timeMeasuredIO: MyIO[Long] = measureTime(anIO_2)

    def printlnIO[A](output: A): MyIO[Unit] = MyIO { () =>
      println(s"Printing from IO: $output")
    }
    def readlnIO(): MyIO[String]            = MyIO { () =>
      scala.io.StdIn.readLine("Input:")
    }

    val printReadLine: MyIO[Unit] = readlnIO().flatMap(line => printlnIO(line))

    println("End of the world happening - Im evaluating IOs now!")
    anIO_1.unsafeRun()
    val timeMeasured: Long = timeMeasuredIO.unsafeRun()
    println(s"Time measured is: $timeMeasured")
    println("Test read/print:")
    printReadLine.unsafeRun()
  }

}
