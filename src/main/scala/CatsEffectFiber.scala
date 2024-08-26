import cats.effect.*
import cats.effect.kernel.Resource.*
import cats.effect.kernel.Resource.ExitCase.{Canceled, Succeeded, Errored}
import cats.syntax.all.*

object Colorize {
  def apply(a: Any): String =
    s"${colors(a.hashCode.abs % numColors)}$a${Console.RESET}"

  def reversed(a: Any): String =
    s"${Console.REVERSED}${apply(a)}"

  private val colors    = List(
    Console.WHITE,
    Console.BLACK + Console.WHITE_B,
    Console.RED,
    Console.GREEN,
    Console.YELLOW,
    Console.BLUE,
    Console.MAGENTA,
    Console.CYAN
  )
  private val numColors = colors.size - 1
}

/** `import com.innerproduct.ee.debug._` to access the `debugWithThread` method.
  */
object debug {

  /** Print to the console the value of the effect along with the thread it was computed on.
    */
  def debugWithThread[A](a: => A): IO[A] =
    WithThreadName(a).debug()

  case class WithThreadName[A] private (value: IO[(A, String)]) {
    def debug(colorize: Boolean = true): IO[A] =
      for {
        atn    <- value
        (a, tn) = atn
        msg     = if (colorize) then s"[${Colorize.reversed(tn)}] $a" else s"[$tn] $a" // <1>
        _      <- IO(msg).debug()
      } yield a
  }

  object WithThreadName {
    def apply[A](a: => A): WithThreadName[A] =
      WithThreadName(IO(a -> Thread.currentThread.getName))
  }
}

import scala.concurrent.duration.*
import debug.*

object JoinAfterStart extends IOApp.Simple {

  val printThread: IO[Unit] =
    IO { Thread.currentThread.getName } flatMap { tn => IO.println(s"Current thread: $tn") }

  def run: IO[Unit] =
    for {
      _     <- printThread
      _     <- debugWithThread("start")
      fiber <- task.start.flatTap(_ => debugWithThread("fiber-start"))
      _     <- debugWithThread("pre-join")

      // _  <- fiber.join.debug() <* printThread
      _     <- debugWithThread("post-join")
      _     <- printThread
      _     <- IO.sleep(4.seconds)
      // _     <- fiber.cancel
    } yield ()

  val task: IO[String] =
    (debugWithThread("task") <* IO.sleep(1.seconds)).foreverM.guarantee(IO.println("Closing"))
}

object ResourceApp extends IOApp.Simple:



  def run: IO[Unit] =
    Resource
      .make { IO.never.guarantee(IO.println("Explicit Background done")).start } { _.cancel }
      .use { _ => IO.never }
      .void

object ResourceApp2 extends IOApp.Simple:
  def run: IO[Unit] =
    IO.never
      .guarantee(IO.println("done background"))
      .background
      .use { _ => IO.never } // _ is the effect of joining a fiber. If ran, you join the fiber
      .void
