import cats.Traverse
import cats.effect.*
import cats.syntax.all.*
import io.circe.*
import io.circe.optics.JsonPath.*
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.parser.*
import monocle.syntax.all.*
import monocle.*

import CirceApp.*






val cpldJson            = parse(jString).getOrElse(Json.Null)
val workJson            = getWork(cpldJson)
val relationAnnotations = getRelationAnnotations(cpldJson)

val relAnnoJson         = relationAnnotations.head
val relOccurrenceJson   = root.`oa:hasBody`.each.json.getAll(relAnnoJson).head
val evidenceId          = makeEvidenceId(relOccurrenceJson)
val evidenceObject      = genEvidenceObject(evidenceId, relOccurrenceJson,relAnnoJson, workJson)

val idsObject = makeIdsObject(workJson)

def makeSentenceIdObject(idsObject: Json, startOffset: String): Json = {
    val idFields =
        idsObject
          .as[Map[String, String]]
          .map(_.toList)
          .getOrElse(List.empty)

    val id = idFields.sortBy {
          case ("biomed:doi", _) => 0
          case ("biomed:pii", _) => 1
          case ("biomed:pui", _) => 2
          case ("biomed:pmid", _) => 3
          case (_, _) => 4
      }
      .headOption.map(_._2)

    id.fold { Json.obj() } { id => Json.obj("biomed:sentenceId" -> s"$id#cont/$startOffset".asJson ) }
}



