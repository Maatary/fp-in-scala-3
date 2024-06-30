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


def makeOccurrenceSpecificAttributeObject(relOccurrenceJson: Json): Json =
    List(
        root.`biomed:cellType`.string.getOption(relOccurrenceJson).fold(Json.obj())(value => Json.obj("biomed:cellType" -> value.asJson)),
        root.`biomed:cellLine`.string.getOption(relOccurrenceJson).fold(Json.obj())(value => Json.obj("biomed:cellLine" -> value.asJson)),
        root.`biomed:tissue`.string.getOption(relOccurrenceJson).fold(Json.obj())(value => Json.obj("biomed:tissue" -> value.asJson)),
        root.`biomed:organism`.string.getOption(relOccurrenceJson).fold(Json.obj())(value => Json.obj("biomed:organism" -> value.asJson)),
        root.`biomed:organ`.string.getOption(relOccurrenceJson).fold(Json.obj())(value => Json.obj("biomed:organ" -> value.asJson)),

        root.`biomed:canonicalId`.string.getOption(relOccurrenceJson).map(value => Json.obj("biomed:support" -> value.asJson)).get,
        root.`biomed:confidence`.string.getOption(relOccurrenceJson).map(value => Json.obj("biomed:confidence" -> value.asJson)).get,
        root.`biomed:effect`.string.getOption(relOccurrenceJson).map(value => Json.obj("biomed:effect" -> value.asJson)).get,
        Json.obj("@type" -> "biomed:Evidence".asJson),
        )
      .foldRight(Json.obj())(_ deepMerge _)





def makeAuthorObject(json: Json): Json =
    val authors = root.`edm:hasAuthor`.each.`edm:label`.string.getAll(json).mkString(", ")
    if (authors.nonEmpty) Json.obj("biomed:authors" -> authors.asJson) else Json.obj()


def makeIdsObject(workJson: Json): Json =
    val ids = root.`edm:identifier`.each.json.foldMap { json =>
        root.`@type`.string.getOption(json).get match {
            case "idtype:DOI"    => List("biomed:doi" ->  root.`@value`.string.getOption(json).get)
            case "idtype:PII"    => List("biomed:pii" ->  root.`@value`.string.getOption(json).get)
            case "idtype:PUI"    => List("biomed:pui" ->  root.`@value`.string.getOption(json).get)
            case "idtype:PUBMED" => List("biomed:pmid" ->  root.`@value`.string.getOption(json).get)
            case _ => List.empty
        }
    }(workJson).toMap.asJson

    ids

def makePageRangeObject(json: Json): Json =
    val startPage = root.`edm:startPage`.string.getOption(json)
    val endPage   = root.`edm:endPage`.string.getOption(json)
    (startPage, endPage) mapN { (start, end) => Json.obj("biomed:pageRange" -> s"$start-$end".asJson) } getOrElse Json.obj()


def makeTitleObject(json: Json): Json =
    root.`edm:title`.string.getOption(json).fold(Json.obj())(value => Json.obj("biomed:title" -> value.asJson))


def makeIssueObject(json: Json): Json =
    root.`edm:issueLabel`.string.getOption(json).fold(Json.obj())(value => Json.obj("biomed:issue" -> value.asJson))


def makeVolumeObject(json: Json): Json =
    root.`edm:partOf`.`edm:volume`.string.getOption(json).fold(Json.obj())(value => Json.obj("biomed:volume" -> value.asJson))


def makeISSNObject(json: Json): Json =
    val issn = root.`edm:partOf`.`edm:identifier`.each.json.foldMap { json =>
        root.`@type`.string.getOption(json).get match {
            case "idtype:eISSN" => List("biomed:e-issn" -> root.`@value`.string.getOption(json).get)
            case "idtype:pISSN" => List("biomed:p-issn" -> root.`@value`.string.getOption(json).get)
            case _ => List.empty
        }
    }(json).toMap.asJson

    issn

def makeJournalTitle(json: Json): Json =
    root.`edm:partOf`.`edm:title`.string.getOption(json).fold(Json.obj())(value => Json.obj("biomed:journalTitle" -> value.asJson))


def makePublishedYearObject(json: Json): Json =
    root.`edm:publishedYear`.int.getOption(json).fold(Json.obj())(value => Json.obj("biomed:publishedYear" -> value.asJson))


def makeEvidenceObject(json: Json): Json =
    val evidence = root.`biomed:evidence`.string.getOption(json)
    evidence.fold(Json.obj())(value => Json.obj("biomed:evidence" -> value.asJson))



val authorsObj      = makeAuthorObject(workJson)
val ids             = makeIdsObject(workJson)
val pageRange       = makePageRangeObject(workJson)
val issue           = makeIssueObject(workJson)
val title           = makeTitleObject(workJson)
val volume          = makeVolumeObject(workJson)
val issn            = makeISSNObject(workJson)
val journalTitle    = makeJournalTitle(workJson)
val publishedYear   = makePublishedYearObject(workJson)
val occurrenceSpecificAttributes = makeOccurrenceSpecificAttributeObject(relOccurrenceJson)

List(journalTitle, volume, issue, issn, pageRange, ids, authorsObj, title, occurrenceSpecificAttributes)
  .foldRight(Json.obj())(_ deepMerge _)




