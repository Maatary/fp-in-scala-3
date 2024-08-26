import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*
import cats.effect.*
import cats.syntax.all.*

object CatEffectIO extends IOApp.Simple:

    def dispatch(dispatcher: Dispatcher[IO], io: IO[Any]): IO[Unit] = IO {
        dispatcher.unsafeRunSync(io)
        IO.unit
    }

    def run: IO[Unit] =

        def tickingClock: IO[Unit] =
            for
                currentTime <- IO.realTime
                _           <- IO.println(s"Current time: $currentTime")
                _           <- IO.sleep(1.second)

                _ <- tickingClock
            yield ()

        tickingClock



        //SyncIO(println("Hello, world!")).unsafeRunSync()
        IO.println("Hello, world!!").syncStep(Int.MaxValue).unsafeRunSync()
        Dispatcher.sequential[IO].use(d => dispatch(d, IO.println("Hello, world! from dispatcher"))).unsafeRunSync()

        IO.unit
