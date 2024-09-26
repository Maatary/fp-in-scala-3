import cats.effect.{ExitCode, IO}
import cats.effect.unsafe.implicits.global

IO{throw RuntimeException("Error")}
    .handleErrorWith(e => IO{e.printStackTrace()})
    .as(ExitCode.Success)
    .unsafeRunSync()