
import cats.syntax.all.given
import cats.effect._
import fs2._
import cats.effect.unsafe.implicits.global


Stream
  .emits(List(1, 2, 3, 4, 5))
  .debugChunks(c => s"step1: $c")
  .evalMap( i => IO.println(i) )
  .debugChunks(c => s"step2: $c")
  .compile
  .drain.unsafeRunSync()



val buf = new scala.collection.mutable.ListBuffer[String]()
Stream.emits(0 until 2 by 1)
      .covary[SyncIO]
      .debugChunks(c => s"step0: $c")
      .map(_ * 2)
      .debugChunks(c => s"step1: $c")
      .evalMap(i => SyncIO { buf += s">$i"; i })  // Logs when elements are pulled from upstream
      .debugChunks(c => s"step2: $c")
      .buffer(4)  // Pulls 4 elements at a time
      .debugChunks(c => s"step3: $c")
      .evalMap(i => SyncIO { buf += s"<$i"; i })  // Logs when elements are processed downstream
      .debugChunks(c => s"step4: $c")
      .take(10)
      .compile.toVector.unsafeRunSync()