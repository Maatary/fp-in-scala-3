//> using lib "org.typelevel::cats-core:2.12.0"
//> using lib "org.typelevel::cats-effect:3.5.4"
//> using lib "io.circe::circe-core:0.14.7"
//> using lib "io.circe::circe-generic:0.14.7"
//> using lib "io.circe::circe-parser:0.14.7"
//> using lib "io.circe::circe-optics:0.15.0"
//> using lib "dev.optics::monocle-core:3.2.0"
//> using lib "dev.optics::monocle-macro:3.2.0"

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

val jString =
    """{
      |            "@id": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S1383576923000454",
      |            "@type": "edm:Work",
      |            "edm:identifier": [
      |                {
      |                    "@type": "idtype:PII",
      |                    "@value": "S1383576923000454"
      |                },
      |                {
      |                    "@type": "idtype:PUI",
      |                    "@value": "2025091111"
      |                },
      |                {
      |                    "@type": "idtype:DOI",
      |                    "@value": "10.1016/j.parint.2023.102768"
      |                },
      |                {
      |                    "@type": "idtype:PUBMED",
      |                    "@value": "dummy PUBMED ID"
      |                }
      |            ],
      |            "edm:title": "Description and development of Auerbachia ignobili n. sp. (Cnidaria: Myxosporea: Bivalvulida) from the giant trevally, Caranx ignobilis (Forsskål, 1775) from Indian waters",
      |            "edm:startPage": "10",
      |            "edm:endPage": "12",
      |            "edm:publishedYear": 2023,
      |            "edm:issueLabel": "Issue number of the Journal where article was published",
      |            "edm:partOf": {
      |                "@type": "edm:Journal",
      |                "edm:title": "Parasitology International",
      |                "edm:identifier": [
      |                    {
      |                        "@type": "idtype:eISSN",
      |                        "@value": "18730329"
      |                    },
      |                    {
      |                        "@type": "idtype:pISSN",
      |                        "@value": "13835769"
      |                    }
      |                ],
      |                "edm:hasEditor": [
      |                    {"@type": "edm:Person", "edm:label": "Hal E. (indexed-name)"},
      |                    {"@type": "edm:Person", "edm:label": "Donald D. (indexed-name)"}
      |                ],
      |                "edm:volume": "96",
      |                "edm:hasPublisher": {"@type": "edm:Organization", "edm:name": "Elsevier Ireland Ltd"}
      |            },
      |            "edm:hasAuthor": [
      |                    {"@type": "edm:Person", "edm:label": "Chandran A."},
      |                    {"@type": "edm:Person", "edm:label": "Surendran S."},
      |                    {"@type": "edm:Person", "edm:label": "Gangadharan S."},
      |                    {"@type": "edm:Person", "edm:label": "Shamal P."},
      |                    {"@type": "edm:Person", "edm:label": "Binesh C.P."},
      |                    {"@type": "edm:Person", "edm:label": "Zacharia P.U."},
      |                    {"@type": "edm:Person", "edm:label": "Sathianandan T.V."},
      |                    {"@type": "edm:Person", "edm:label": "Sanil N.K."}
      |            ]
      |        }
      """.stripMargin



val json = parse(jString).getOrElse(Json.Null)

def makeAuthorObject(json: Json): Json =
    val authors = root.`edm:hasAuthor`.each.`edm:label`.string.getAll(json).mkString(", ")
    if (authors.nonEmpty) Json.obj("biomed:authors" -> authors.asJson) else Json.obj()

    
def makeIdsObject(json: Json): Json =
    val ids = root.`edm:identifier`.each.json.foldMap { json =>
        root.`@type`.string.getOption(json).get match {
            case "idtype:DOI"    => List("biomed:doi" ->  root.`@value`.string.getOption(json).get)
            case "idtype:PII"    => List("biomed:pii" ->  root.`@value`.string.getOption(json).get)
            case "idtype:PUI"    => List("biomed:pui" ->  root.`@value`.string.getOption(json).get)
            case "idtype:PUBMED" => List("biomed:pmid" ->  root.`@value`.string.getOption(json).get)
            case _ => List.empty
        }
    }(json).toMap.asJson

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



val authorsObj      = makeAuthorObject(json)
val ids             = makeIdsObject(json)
val pageRange       = makePageRangeObject(json)
val issue           = makeIssueObject(json)
val title           = makeTitleObject(json)
val volume          = makeVolumeObject(json)
val issn            = makeISSNObject(json)
val journalTitle    = makeJournalTitle(json)
val publishedYear   = makePublishedYearObject(json)


List(journalTitle, volume, issue, issn, pageRange, ids, authorsObj, title)
  .foldRight(JsonObject.empty.asJson)(_ deepMerge _)




