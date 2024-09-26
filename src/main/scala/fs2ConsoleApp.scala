import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import fs2.*

import scala.concurrent.duration.DurationInt






object fs2ConsoleApp extends IOApp.Simple:
    override def run: IO[Unit] =

        val ref = Stream.eval(Ref[IO].of[List[String]](Nil))

        ref.flatMap: ref =>
            Stream
            .eval(Console[IO].println("Hello, enter a value to emit"))
            .evalMap(_ =>  Console[IO].readLine)
            .evalTap(line => IO.println(s"Thanks for inputting $line"))
            .repeat
            .evalTap(line => ref.update ( line::_ ) )
            .take(3)
            .last
            .evalTap(_ => ref.get flatMap(l => IO.println(s"Thanks for inputting $l")) )
        .compile
        .drain
        .as(ExitCode.Success)





