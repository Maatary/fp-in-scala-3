
import fs2.*
import cats.effect.*
import fs2.io.file.*
import fs2.io.file.Files.*
import fs2.io.file.{Files, Path}
import cats.syntax.all.given
import cats.effect.unsafe.implicits.given
import scala.concurrent.duration.*


/**
 *  Take return Pull.Done internally, so it will not emit any element.
 *  Hence after take, we have a stream program that only emit one value
 *  Repeating this program will only emit one value each time
 *
 *  Need to be executed in Plain
 */

Stream
    .eval(Files[IO].currentWorkingDirectory)
    .flatMap { path => Files[IO].list(path) }
    .map { path => path.toString }
    .take(1)
    .repeat
    .metered(1.second)
    .evalTap { path => IO.println(path) }
    .compile
    .drain
    .unsafeRunSync()






