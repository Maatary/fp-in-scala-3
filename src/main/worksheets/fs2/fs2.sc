
import cats.syntax.all.given
import cats.effect._
import fs2._
import cats.effect.unsafe.implicits.global


Stream
  .emits(List(1, 2, 3, 4, 5))
  .evalMap( i => IO.println(i) )
  .compile
  .drain.unsafeRunSync()


IO{throw RuntimeException("Error")}
  .handleErrorWith(e => IO{e.printStackTrace()})
  .as(ExitCode.Success)
  .unsafeRunSync()