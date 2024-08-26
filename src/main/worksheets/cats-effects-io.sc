
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global

import scala.concurrent.duration.*
import cats.effect.{IO, Resource, SyncIO}


val program1 =
  for
    target <- IO.println("Catch me if you can!").foreverM.start
    _ <- IO.sleep(10.milliseconds)
    _ <- target.cancel
  yield ()

//program1.unsafeRunSync()


Dispatcher.sequential[IO].use { dispatcher =>
    dispatcher.unsafeRunAndForget(IO.println("Catch me if you can gain!"))
    IO.unit
}.unsafeRunSync()


SyncIO("").unsafeRunSync()


IO("").unsafeRunSync()


Option(1).flatMap(x => Option(x + 1))