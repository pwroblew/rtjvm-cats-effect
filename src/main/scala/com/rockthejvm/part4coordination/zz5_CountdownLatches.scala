package com.rockthejvm.part4coordination

import cats.effect.std.CountDownLatch
import cats.effect.{Deferred, IO, IOApp, Ref, Resource}
import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxParallelTraverse1, toTraverseOps}
import com.rockthejvm.utils.*

import java.io.{File, FileWriter}
import scala.concurrent.duration.DurationInt
import scala.io.{BufferedSource, Source}
import scala.util.Random

object zz5_CountdownLatches extends IOApp.Simple {

  def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO("starting the race shortly").debug2 >> IO.sleep(2.seconds)
    _ <- IO("5....").debug2 >> IO.sleep(1.second) >> latch.release
    _ <- IO("4....").debug2 >> IO.sleep(1.second) >> latch.release
    _ <- IO("3....").debug2 >> IO.sleep(1.second) >> latch.release
    _ <- IO("2....").debug2 >> IO.sleep(1.second) >> latch.release
    _ <- IO("1....").debug2 >> IO.sleep(1.second) >> latch.release
    _ <- IO("GO GO GO!").debug2
  } yield ()

  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"[runner $id] waiting for signal...").debug2
    _ <- latch.await
    _ <- IO(s"[runner $id] Running!! ").debug2
  } yield ()

  def raceDemo() = for {
    latch        <- CountDownLatch[IO](5)
    announcerFib <- announcer(latch).start
    _            <- (1 to 10).toList.parTraverse(id => createRunner(id, latch))
    _            <- announcerFib.join
  } yield ()

  // exercise

  object FileServer {
    val fileChunkList: List[String] = List(
      "I love Scala",
      "Cats Effect seems quite fun",
      "Never would I have thought that I would do low-level concurrency programming WITH pure FP"
    )

    def getNumChunks: IO[Int]            = IO(fileChunkList.length)
    def getFileChunk(n: Int): IO[String] =
      IO(s"[server] starting providing the $n'th part of the file'").debug2
        >> IO.sleep(Random.nextInt(2000).millis)
        >> IO(s"[server] $n'th part sent'").debug2
        >> IO(fileChunkList(n))
  }

  def writeToFile(path: String, contents: String): IO[Unit] = {
    val fileResource: Resource[IO, FileWriter] =
      Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))

    fileResource.use(writer => IO(writer.write(contents)))
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource: Resource[IO, (BufferedSource, FileWriter)] = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(reader => IO(reader.close()))
      writer <-
        Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)

    compositeResource.use { (reader, writer) =>
      IO(reader.getLines().foreach(line => writer.write(line + "\n")))
    }
  }

  def downloadFile(fileName: String, destFolder: String): IO[Unit] = for {
    chunksNum <- FileServer.getNumChunks
    // latch     <- CountDownLatch[IO](chunksNum)
    latch     <- MyCountDownLatch(chunksNum)
    _         <- (1 to chunksNum).toList.parTraverse { ind =>
                   for {
                     _     <- IO(s"[receiver $ind] starting download").debug2
                     partN <- FileServer.getFileChunk(ind - 1)
                     _     <- writeToFile(destFolder + s"/tmp$ind.txt", partN)
                     _     <- latch.release
                     _     <- IO(s"[receiver $ind] download completed").debug2
                   } yield ()
                 }
    _         <- latch.await
    _         <- (1 to chunksNum).toList.map(ind => destFolder + s"/tmp$ind.txt").traverse(fromFile =>
                   appendFileContents(fromFile, s"$destFolder/$fileName")
                 )
  } yield ()

  override def run: IO[Unit] =
    IO.unit
      // >> raceDemo()
      >> downloadFile("scala.txt", "src/main/resources")
}

/*
implement countdownlatch with ref and deferred.
 */

class MyCountDownLatch(counter: Ref[IO, Int], signal: Deferred[IO, Unit]) {

  def release: IO[Unit] = for {
    countVal <- counter.updateAndGet(_ - 1)
    _        <- if (countVal < 1) signal.complete(()) else IO.unit
  } yield ()

  def await: IO[Unit] = signal.get
}

object MyCountDownLatch {
  def apply(n: Int): IO[MyCountDownLatch] = for {
    counter <- IO.ref(n)
    signal  <- IO.deferred[Unit]
  } yield new MyCountDownLatch(counter, signal)
}
