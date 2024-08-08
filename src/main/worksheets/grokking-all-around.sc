import scala.util.chaining.scalaUtilChainingOps

import cats.effect.unsafe.implicits.given // the given import all the implicits
import cats.syntax.all.given // the given import all the implicits
import cats.effect.*

import scala.io.StdIn





type ResourceType = String





sealed trait GraphObject
case object CanonicalRelation extends GraphObject
case object CanonicalEntity extends GraphObject
case object Evidence extends GraphObject


abstract class GraphEvent[ + A <: GraphObject](val key: String, val value: A)
case class CanonicalEntityEvent(override val key: String, override val value: CanonicalEntity.type) extends GraphEvent[CanonicalEntity.type](key, value)
case class CanonicalRelationEvent(override val key: String, override val value: CanonicalRelation.type) extends GraphEvent[CanonicalRelation.type](key, value)
case class EvidenceEvent(override val key: String, override val value: Evidence.type) extends GraphEvent(key, value)



EvidenceEvent("hello", Evidence)

val f = (x: GraphEvent[GraphObject]) => x match {
  case CanonicalEntityEvent(key, value) => x.value.toString
  case CanonicalRelationEvent(key, value) => x.value.toString
  case EvidenceEvent(key, value) => x.value.toString
  case _ => x.value.toString
}


//f(EvidenceEvent("hello", Evidence))

val e: GraphEvent[GraphObject] = EvidenceEvent("hello", Evidence)


val json: String = """
  {
    "id": "c730433b-082c-4984-9d66-855c243266f0",
    "name": "Foo",
    "counts": [1, 2, 3],
    "values": {
      "bar": true,
      "baz": 100.001,
      "qux": ["a", "b"]
    }
  }
"""

import io.circe._, io.circe.parser._
import io.circe.parser.given

val jsonR = IO.fromEither(parse(json))

val op =
        jsonR flatTap: json =>
          if (json.hcursor.downField("id").succeeded) IO.println(json.spaces2)
          else IO.println("field does not exist")

op.unsafeRunSync()


val state =

    for
        ref <- Ref[IO].of(10)
        _ <- ref.update(_ + 2)
        value <- ref.get
    yield value

state.unsafeRunSync() tap {println(_) }

(None:Option[Int], Some(2)).mapN(_ + _)