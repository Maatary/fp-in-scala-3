
import fs2.*
import cats.effect.*
import fs2.io.file.*
import fs2.io.file.Files.*
import fs2.io.file.{Files, Path}
import cats.syntax.all.given
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*




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


