package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp, Resource}
import com.rockthejvm.utils.*

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object L02_Resources extends IOApp.Simple {

  class Connection(url: String) {
    def open: IO[String]  = IO(s"conn: opening connection to $url").debug2
    def close: IO[String] = IO(s"conn: closing connection to $url").debug2
  }

  val firstUrl                = "rockthejvm.com"
  val asyncFetchUrl: IO[Unit] = for {
    fib1 <- IO(s"effect: creating connection naively to $firstUrl").debug2 >>
              (new Connection(firstUrl).open >> IO.sleep(Int.MaxValue.seconds)).start
    _    <- IO.sleep(1.second) >> fib1.cancel
  } yield ()
  // leaking resources

  // the solution
  val secondUrl                      = "blablabla.com"
  val correctAsyncFetchUrl: IO[Unit] = for {
    conn <- IO(s"effect: creating connection to $secondUrl and using onCancel").debug2
              >> IO.pure(new Connection(secondUrl))
    fib1 <- (conn.open >> IO.sleep(Int.MaxValue.seconds))
              .onCancel(conn.close.void)
              .start
    _    <- IO.sleep(1.second) >> fib1.cancel
  } yield ()
  // but tedious

  // next solution: bracket pattern
  val thirdUrl                               = "dfwef.com"
  private val bracketAsyncFetchUrl: IO[Unit] = IO.pure(new Connection(thirdUrl))
    .bracket(conn => conn.open >> IO.sleep(Int.MaxValue.seconds))(_.close.void)
  // IO(__aquire__).bracket(__use__)(__release__)
  // this is the FP equivalent of try/catch

  val bracketProgram: IO[Unit] = for {
    _    <- IO(s"effect: creating connection to $thirdUrl with the bracket pattern").debug2
    fib1 <- bracketAsyncFetchUrl.start
    _    <- IO.sleep(500.millis) >> fib1.cancel
  } yield ()

  /*
  Exercise
   */

  def openFileScanner(path: String): IO[Scanner] =
    IO(s"scanner: creating new for $path").debug2 >> IO(new Scanner(new FileReader(new File(path))))

  def readingWithScanner(scanner: Scanner): IO[Unit] =
    if (scanner.hasNext) IO.blocking("scanner: next line: " + scanner.nextLine()).debug2.void
    else IO.unit

  def usingScanner(scanner: Scanner): IO[Unit] =
    (IO.sleep(100.millis) >> readingWithScanner(scanner)).foreverM.void

  def bracketReadFile(path: String): IO[Unit] =
    openFileScanner(path).bracket(usingScanner) { scanner =>
      IO(s"scanner: closing for $path").debug2 >> IO(scanner.close())
    }

  val path1                       = "src/main/scala/com/rockthejvm/part3concurrency/L02_Resources.scala"
  val bracketProgram_v2: IO[Unit] = for {
    fib1 <- bracketReadFile(path1).start
    _    <- IO.sleep(700.millis) >> fib1.cancel
    // _    <- fib1.join
  } yield ()

  // problem with bracket - using/chaining more than 1 resources
  case class URL(url: String)
  class FileResource(path: String) {
    def close: IO[Unit]                       = IO(s"file: closing for $path").debug2.void
    def getUrl(implicit url: URL): IO[String] = IO.pure(url.url)
  }
  object FileResource              {
    def open(path: String): IO[FileResource] =
      IO(s"file: opening $path").debug2 >> IO.pure(new FileResource(path))
  }

  class ConnectionResource(url: String) {
    def close: IO[Unit]                 = IO(s"conn: closing conn to $url").debug2.void
    def send(message: String): IO[Unit] = IO(s"conn: sending: $message").debug2.void
  }
  object ConnectionResource             {
    def open(url: String): IO[ConnectionResource] =
      IO(s"conn: opening conn to $url").debug2 >> IO.pure(new ConnectionResource(url))
  }

  def usingConnection(conn: ConnectionResource): IO[Unit] = for {
    _ <- (conn.send("sending via connection") >> IO.sleep(250.millis)).replicateA_(4)
  } yield ()

  val path2              = "this/is/just/mock/path.json"
  implicit val url2: URL = URL("bulba.com")

  private val bracketProgram_v3: IO[Unit] =
    FileResource.open(path2).bracket { file =>
      file.getUrl.flatMap { url =>
        ConnectionResource.open(url).bracket(usingConnection) { _.close }
      }
    } { _.close }
  // this nesting above is tedious.... again

  // solution .... ==> ... Resource
  def connectionResource(url: String): Resource[IO, ConnectionResource] =
    Resource.make(ConnectionResource.open(url))(_.close)
  def fileResource(path: String): Resource[IO, FileResource]            =
    Resource.make(FileResource.open(path))(_.close)

  val resourceProgram: IO[Unit] = for {
    url <- fileResource(path2).use(_.getUrl)
    _   <- connectionResource(url).use(usingConnection)
  } yield ()

  val oneBigResource: Resource[IO, ConnectionResource] = for {
    fileR <- fileResource(path2)
    url   <- Resource.eval(fileR.getUrl)
    connR <- connectionResource(url)
  } yield connR
  val resourceProgram_v2: IO[Unit]                     = oneBigResource.use(usingConnection)

  /*
  exercise
   */

  def fileScannerResource(path: String): Resource[IO, Scanner] =
    Resource.make(openFileScanner(path)) { scanner =>
      IO(s"scanner: closing for $path").debug2 >> IO(scanner.close())
    }

  val resourceProgram_v3: IO[Unit] = for {
    fib1 <- fileScannerResource(path1).use(usingScanner).start
    _    <- IO.sleep(3.seconds) >> fib1.cancel
  } yield ()

  val ioFinalizers: IO[Unit] = {
    IO("some resource").debug2.guarantee(IO.println("finzlizer 1!")).void
    IO.raiseError[Unit](
      new RuntimeException("FATAL ERROR")
    ).guarantee(IO.println("finzlizer 2!")).void
  }

  override def run: IO[Unit] = for {
    _ <- IO.println("asyncFetchUrl -------------------------------------")
    _ <- asyncFetchUrl
    _ <- IO.println("correctAsyncFetchUrl -------------------------------------")
    _ <- correctAsyncFetchUrl
    _ <- IO.println("bracketProgram -------------------------------------")
    _ <- bracketProgram
    _ <- IO.println(" bracketProgram_v2-------------------------------------")
    _ <- bracketProgram_v2
    _ <- IO.println(" bracketProgram_v3-------------------------------------")
    _ <- bracketProgram_v3
    _ <- IO.println(" resourceProgram-------------------------------------")
    _ <- resourceProgram
    _ <- IO.println("resourceProgram_v2 -------------------------------------")
    _ <- resourceProgram_v2
    _ <- IO.println("resourceProgram_v3 -------------------------------------")
    _ <- resourceProgram_v3
    _ <- IO.println("finalizers -------------------------------------")
    _ <- ioFinalizers
    _ <- IO.println("-------------------------------------")
  } yield ()

}
