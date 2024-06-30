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
val relOccurrenceJson   = root.`oa:hasBody`.each.json.getAll(relationAnnotations.head).head

val evidenceId          = makeEvidenceId(relOccurrenceJson)


val evidenceObject = genEvidenceObject(evidenceId, relOccurrenceJson, workJson)




