import cats.effect.*
import cats.syntax.all.*
import io.circe.*
import io.circe.optics.JsonPath.*
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.parser.*
import monocle.syntax.all.*
import monocle.*

import scala.util.chaining.scalaUtilChainingOps

type CPLDJson                       = Json

type EntityJson                     = Json
type EntityAnnoJson                 = Json
type EntityAnnoJsonMap              = Map[String, EntityAnnoJson]

type RelationJson                   = Json
type RelationAnnoJson               = Json
type RelationAnnoJsonMap            = Map[String, RelationAnnoJson]

type WorkJson                       = Json

type CanonicalRelationJson          = Json
type CanonicalRelationSubGraphJson  = Json


object CirceApp extends IOApp.Simple {


    def getEntityAnnotationsMap(json: CPLDJson): Map[String, EntityAnnoJson] =
        root.`@graph`.each
          .filter(root.`@type`.string.getOption(_).contains("biomce:EntityAnnotation"))
          .json
          .foldMap { json =>
              val id = root.`@id`.string.getOption(json).get
              List(id -> json)
          }(json).toMap

    def getRelationAnnotations(cpldJson: CPLDJson): List[RelationAnnoJson] =
        root.`@graph`.each
          .filter(root.`@type`.string.getOption(_).contains("biomce:RelationAnnotation"))
          .json
          .foldMap { List(_) }(cpldJson)


    def getWork(cpldJson: CPLDJson): WorkJson =
        root.`@graph`.each
          .filter(root.`@type`.string.getOption(_).contains("edm:Work"))
          .json
          .headOption(cpldJson).get


    def LookupEntityObjects(EntityAnnoIds: List[String], entityAnnoMap: Map[String, EntityAnnoJson]): List[EntityJson] =

        EntityAnnoIds.flatMap(entityAnnoMap.get).map { json =>
            root.`oa:hasBody`.arr.index(0).getOption(json).get
        }


    def genCanonicalRelation(relOccurrenceJson: Json): CanonicalRelationJson =

        val id          = root.`@id`.string.getOption(relOccurrenceJson).get
        val canonicalId = root.`biomed:canonicalId`.string.getOption(relOccurrenceJson).get
        val evidenceId  = id.replace("biodata:occurrence", "biodata:evidence")

        relOccurrenceJson.asObject.get.filterKeys {
              case "biomed:regulator" | "biomed:target" | "biomed:effect" | "biomed:partner"
                   | "@type" => true
              case _ => false
          }
          .+:("@id", canonicalId.asJson)
          .add("biomed:evidence", List(evidenceId).asJson)
          .asJson

    def genCanonicalRelationSubGraphs(relAnnoJson: RelationAnnoJson, entityAnnoMap: Map[String, EntityAnnoJson]): List[CanonicalRelationSubGraphJson] =

        val entityAnnoIds = root.`prov:wasInformedBy`.each.string.getAll(relAnnoJson)
        val entityObjects = LookupEntityObjects(entityAnnoIds, entityAnnoMap)

        root.`oa:hasBody`.each.json.getAll(relAnnoJson) map { relOccurrenceJson =>
            val rel = genCanonicalRelation(relOccurrenceJson)
            //make evidence
            JsonObject("@graph" ->  (rel::entityObjects).asJson).asJson
        }


    val jString =
        """
          |{
          |    "@context": [
          |        "https://data.elsevier.com/lifescience/context/biomed/biomce.jsonld",
          |        {
          |            "doc": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X#"
          |        }
          |    ],
          |    "@graph": [
          |        {
          |            "@id": "https://data.elsevier.com/lifescience/biomed/instance/testdata/content-enrichment/directed-relation-annotation/valid/sample2",
          |            "@type": "owl:Ontology",
          |            "owl:imports": "https://data.elsevier.com/lifescience/schema/biomed/content-enrichment",
          |            "rdfs:comment": "The annotations in this sample are not real and have been drafted to show a complete CPLD model. This sample presents a directed relation. Also, the owl:Ontology element (the current element) here is not part of the actual CPLD model. This is added for working with TopBraid Composer."
          |        },
          |        {
          |            "@id": "doc:relation-annotation-e30cf58db0364c448532525bb4aa06c9-32975_33109",
          |            "@type": "biomce:RelationAnnotation",
          |            "oa:hasBody": [
          |                {
          |                    "@id": "biodata:occurrence/quantitativechange/e30cf58db0364c448532525bb4aa06c9-32975_33109",
          |                    "@type": "biomed:QuantitativeChange",
          |                    "biomed:canonicalId": "biodata:quantitativechange/regulator/D003967/target/MSCAN_1441917",
          |                    "biomed:confidence": "0.004499007482081652",
          |                    "biomed:regulator": "biodata:disease/D003967",
          |                    "biomed:target": "biodata:smallmol/MSCAN_1441917",
          |                    "biomed:organism": "biodata:organism/D006801",
          |                    "biomed:tissue": "biodata:tissue/U0001977",
          |                    "biomed:organ": "biodata:organ/MSCAN_8802986",
          |                    "biomed:cellLine": "biodata:cellLine/CVCL_RA75",
          |                    "biomed:cellType": "biodata:cellType/CELLTYP_218",
          |                    "biomed:effect": "Positive"
          |                }
          |            ],
          |            "oa:motivatedBy": "oa:identifying",
          |            "oa:hasTarget": {
          |                "@type": "oa:ResourceSelection",
          |                "oa:hasSelector": {
          |                    "@type": "oa:XPathSelector",
          |                    "value": "/html/body",
          |                    "oa:refinedBy": {
          |                        "@type": "biomce:SentenceSelector",
          |                        "oa:exact": "In each group, the most common adverse event was diarrhea (tenapanor group: 74.4%, placebo group: 19.5%) (Table 1                   ).",
          |                        "oa:end": 33109,
          |                        "oa:start": 32975
          |                    }
          |                },
          |                "oa:hasSource": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X"
          |            },
          |            "prov:wasInformedBy": [
          |                "doc:entity-annotation-0_9",
          |                "doc:entity-annotation-10184_10192",
          |                "doc:entity-annotation-95818_95823",
          |                "doc:entity-annotation-95826_95837",
          |                "doc:entity-annotation-95605_95615",
          |                "doc:entity-annotation-85551_85555",
          |                "doc:entity-annotation-75748-75762"
          |            ]
          |        },
          |        {
          |            "@id": "doc:entity-annotation-75748-75762",
          |            "@type": "biomce:EntityAnnotation",
          |            "oa:hasBody": [
          |                {
          |                    "@id": "biodata:cellType/CELLTYP_218",
          |                    "@type": "biomed:CellType",
          |                    "skos:prefLabel": "pericyte cell"
          |                }
          |            ],
          |            "oa:motivatedBy": "oa:identifying",
          |            "oa:hasTarget": {
          |                "@type": "oa:ResourceSelection",
          |                "oa:hasSelector": {
          |                    "@type": "oa:XPathSelector",
          |                    "value": "/html/body",
          |                    "oa:refinedBy": {
          |                        "@type": "biomce:SentenceSelector",
          |                        "oa:exact": "pericyte cell",
          |                        "oa:end": 75762,
          |                        "oa:start": 75748
          |                    }
          |                },
          |                "oa:hasSource": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X"
          |            }
          |        },
          |        {
          |            "@id": "doc:entity-annotation-85551_85555",
          |            "@type": "biomce:EntityAnnotation",
          |            "oa:hasBody": [
          |                {
          |                    "@id": "biodata:cellLine/CVCL_RA75",
          |                    "@type": "biomed:CellLine",
          |                    "skos:prefLabel": "AR: 5"
          |                }
          |            ],
          |            "oa:motivatedBy": "oa:identifying",
          |            "oa:hasTarget": {
          |                "@type": "oa:ResourceSelection",
          |                "oa:hasSelector": {
          |                    "@type": "oa:XPathSelector",
          |                    "value": "/html/body",
          |                    "oa:refinedBy": {
          |                        "@type": "biomce:SentenceSelector",
          |                        "oa:exact": "AR: 5",
          |                        "oa:end": 85555,
          |                        "oa:start": 85551
          |                    }
          |                },
          |                "oa:hasSource": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X"
          |            }
          |        },
          |        {
          |            "@id": "doc:entity-annotation-95605_95615",
          |            "@type": "biomce:EntityAnnotation",
          |            "oa:hasBody": [
          |                {
          |                    "@id": "biodata:organ/MSCAN_8802986",
          |                    "@type": "biomed:Organ",
          |                    "skos:prefLabel": "Bile Duct"
          |                }
          |            ],
          |            "oa:motivatedBy": "oa:identifying",
          |            "oa:hasTarget": {
          |                "@type": "oa:ResourceSelection",
          |                "oa:hasSelector": {
          |                    "@type": "oa:XPathSelector",
          |                    "value": "/html/body",
          |                    "oa:refinedBy": {
          |                        "@type": "biomce:SentenceSelector",
          |                        "oa:exact": "bile ducts",
          |                        "oa:end": 95615,
          |                        "oa:start": 95605
          |                    }
          |                },
          |                "oa:hasSource": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X"
          |            }
          |        },
          |        {
          |            "@id": "doc:entity-annotation-0_9",
          |            "@type": "biomce:EntityAnnotation",
          |            "oa:hasBody": [
          |                {
          |                    "@id": "biodata:smallmol/MSCAN_1441917",
          |                    "@type": "biomed:SmallMol",
          |                    "skos:prefLabel": "tenapanor"
          |                }
          |            ],
          |            "oa:motivatedBy": "oa:identifying",
          |            "oa:hasTarget": {
          |                "@type": "oa:ResourceSelection",
          |                "oa:hasSelector": {
          |                    "@type": "oa:XPathSelector",
          |                    "value": "/html/body",
          |                    "oa:refinedBy": {
          |                        "@type": "biomce:SentenceSelector",
          |                        "oa:exact": "Tenapanor",
          |                        "oa:end": 9,
          |                        "oa:start": 0
          |                    }
          |                },
          |                "oa:hasSource": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X"
          |            }
          |        },
          |        {
          |            "@id": "doc:entity-annotation-10184_10192",
          |            "@type": "biomce:EntityAnnotation",
          |            "oa:hasBody": [
          |                {
          |                    "@id": "biodata:disease/D003967",
          |                    "@type": "biomed:Disease",
          |                    "skos:prefLabel": "Diarrhea"
          |                }
          |            ],
          |            "oa:motivatedBy": "oa:identifying",
          |            "oa:hasTarget": {
          |                "@type": "oa:ResourceSelection",
          |                "oa:hasSelector": {
          |                    "@type": "oa:XPathSelector",
          |                    "value": "/html/body",
          |                    "oa:refinedBy": {
          |                        "@type": "biomce:SentenceSelector",
          |                        "oa:exact": "Diarrhea",
          |                        "oa:end": 10192,
          |                        "oa:start": 10184
          |                    }
          |                },
          |                "oa:hasSource": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X"
          |            }
          |        },
          |        {
          |            "@id": "doc:entity-annotation-95818_95823",
          |            "@type": "biomce:EntityAnnotation",
          |            "oa:hasBody": [
          |                {
          |                    "@id": "biodata:organism/D006801",
          |                    "@type": "biomed:Organism",
          |                    "skos:prefLabel": "Human"
          |                }
          |            ],
          |            "oa:motivatedBy": "oa:identifying",
          |            "oa:hasTarget": {
          |                "@type": "oa:ResourceSelection",
          |                "oa:hasSelector": {
          |                    "@type": "oa:XPathSelector",
          |                    "value": "/html/body",
          |                    "oa:refinedBy": {
          |                        "@type": "biomce:SentenceSelector",
          |                        "oa:exact": "Human",
          |                        "oa:end": 95823,
          |                        "oa:start": 95818
          |                    }
          |                },
          |                "oa:hasSource": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X"
          |            }
          |        },
          |        {
          |            "@id": "doc:entity-annotation-95826_95837",
          |            "@type": "biomce:EntityAnnotation",
          |            "oa:hasBody": [
          |                {
          |                    "@id": "biodata:tissue/U0001977",
          |                    "@type": "biomed:Tissue",
          |                    "skos:prefLabel": "blood serum"
          |                }
          |            ],
          |            "oa:motivatedBy": "oa:identifying",
          |            "oa:hasTarget": {
          |                "@type": "oa:ResourceSelection",
          |                "oa:hasSelector": {
          |                    "@type": "oa:XPathSelector",
          |                    "value": "/html/body",
          |                    "oa:refinedBy": {
          |                        "@type": "biomce:SentenceSelector",
          |                        "oa:exact": "blood serum",
          |                        "oa:end": 95837,
          |                        "oa:start": 95826
          |                    }
          |                },
          |                "oa:hasSource": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X"
          |            }
          |        },
          |        {
          |            "@id": "https://data.elsevier.com/lifescience/entellect/normalizedDocument/S027263862300673X",
          |            "@type": "edm:Work",
          |            "edm:identifier": [
          |                {
          |                    "@type": "idtype:PII",
          |                    "@value": "S027263862300673X"
          |                },
          |                {
          |                    "@type": "idtype:PUI",
          |                    "@value": "2025804519"
          |                },
          |                {
          |                    "@type": "idtype:DOI",
          |                    "@value": "10.1016/j.ejphar.2023.175750"
          |                },
          |                {
          |                    "@type": "idtype:PUBMED",
          |                    "@value": "37147146"
          |                }
          |            ],
          |            "edm:title": "Tenapanor for the Treatment of Hyperphosphatemia in Japanese Hemodialysis Patients: A Randomized Phase 3 Monotherapy Study With an Up-titration Regimen",
          |            "edm:startPage": "10",
          |            "edm:endPage": "12",
          |            "edm:publishedYear": 2023,
          |            "edm:issueLabel": "Issue number of the Journal where article was published",
          |            "edm:partOf": {
          |                "@type": "edm:Journal",
          |                "edm:title": "A Journal Title",
          |                "edm:identifier": [
          |                    {
          |                        "@type": "idtype:eISSN",
          |                        "@value": "18790712"
          |                    },
          |                    {
          |                        "@type": "idtype:pISSN",
          |                        "@value": "00142999"
          |                    }
          |                ],
          |                "edm:hasEditor": [
          |                    {
          |                        "@type": "edm:Person",
          |                        "edm:initials": "H.",
          |                        "edm:givenName": "Hal",
          |                        "edm:familyName": "Editor",
          |                        "edm:emailAddress": "hale@email.com",
          |                        "edm:label": "Hal E. (indexed-name)"
          |                    },
          |                    {
          |                        "@type": "edm:Person",
          |                        "edm:initials": "D.",
          |                        "edm:givenName": "Donald",
          |                        "edm:familyName": "Duck",
          |                        "edm:emailAddress": "dodk@email.com",
          |                        "edm:label": "Donald D. (indexed-name)"
          |                    }
          |                ],
          |                "edm:volume": "Volume of Journal",
          |                "edm:hasPublisher": {
          |                    "@type": "edm:Organization",
          |                    "edm:name": "Elsevier B.V."
          |                }
          |            },
          |            "edm:hasAuthor": [
          |                {
          |                    "@type": "edm:Person",
          |                    "edm:initials": "D.",
          |                    "edm:givenName": "Donald",
          |                    "edm:familyName": "Duck",
          |                    "edm:emailAddress": "dodk@email.com",
          |                    "edm:label": "Donald D. (indexed-name)"
          |                },
          |                {
          |                    "@type": "edm:Person",
          |                    "edm:initials": "D.",
          |                    "edm:givenName": "Daisy",
          |                    "edm:familyName": "Duck",
          |                    "edm:emailAddress": "dadk@email.com",
          |                    "edm:label": "Daisy D. (indexed-name)"
          |                }
          |            ]
          |        }
          |    ]
          |}
          |""".stripMargin





    override def run: IO[Unit] = {

        val cpldJson               = parse(jString).getOrElse(Json.Null)
        val entityAnnotationMap    = getEntityAnnotationsMap(cpldJson)
        val workJson               = getWork(cpldJson)
        val relationAnnotations    = getRelationAnnotations(cpldJson)

        val canonicalRelSubGraphs  = genCanonicalRelationSubGraphs(relationAnnotations.head, entityAnnotationMap)


        IO.println(canonicalRelSubGraphs)
          .as(IO.unit)
        


  }
}
