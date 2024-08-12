import scala.util.chaining.scalaUtilChainingOps

object IORefPlay extends App {

  import cats.effect.unsafe.implicits.given
  import cats.syntax.all.*
  import cats.effect.*

  val state =
    for
      ref   <- Ref[SyncIO].of(10)
      _     <- ref.update(_ + 2)
      value <- ref.get
    yield value

  state.unsafeRunSync() tap { println(_) }

}
