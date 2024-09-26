import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import fs2.*

import scala.concurrent.duration.DurationInt

/**
 * This Stream batch the input in a ref and output it at the end
 * Ref is potentially a better solution than using scan
 * Scan is not effectful, so it is not suitable for this use case
 * There are other way of batching e.g. Chunkn.map(_.last).unNone
 */

object fs2ConsoleApp1 extends IOApp.Simple:
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
            .evalTap(_ => ref.get flatMap(l => IO.println(s"Batched Output is $l")) )
        .compile
        .drain
        .as(ExitCode.Success)


/**
 * Ask Indefinitely
 * Batch the input in a ref in batch of 3
 * Could very well use scan with a list instead of ref
 * Indeed Ref work with immutable data structure
 * Ref is not for mutable data structure
 */
object fs2ConsoleApp2 extends IOApp.Simple:
    override def run: IO[Unit] =

        val ref = Stream.eval(Ref[IO].of[List[String]](Nil))

        ref.flatMap: ref =>
               Stream
                 .eval(Console[IO].println("Hello, enter a value to emit"))
                 .evalMap(_ =>  Console[IO].readLine)
                 .evalTap(line => IO.println(s"Thanks for inputting $line"))
                 .evalTap(line => ref.update ( line::_ ) >> IO.pure(1))
                 .repeat // without this ChunkN(.., false) will output done i.e. not emit anything and the overall stream will just be done
                 .chunkN(3, false)
                 .debugChunks(c => s"step1: $c")
                 .evalTap(_ => ref.get flatMap(l => IO.println(s"Batched Output is $l")) )
           .compile
           .drain
           .as(ExitCode.Success)





