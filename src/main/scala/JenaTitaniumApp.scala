import cats.effect.{IO, IOApp}
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.sparql.core.DatasetGraphFactory
import org.apache.jena.riot.system.JenaTitanium
import com.apicatalog.jsonld.{JsonLd, JsonLdOptions}
import com.apicatalog.jsonld.document.{JsonDocument, RdfDocument}
import jakarta.json.Json
import jakarta.json.stream.JsonGenerator

import scala.jdk.CollectionConverters.*
import java.io.{FileInputStream, FileOutputStream, InputStream, OutputStream, StringWriter}
import java.net.URL




object JenaTitaniumApp extends IOApp.Simple:

    extension (file: String)
        def getResource: URL = getClass.getResource(file)
        def getStream: InputStream = getClass.getResourceAsStream(file)


    def run: IO[Unit] =

        IO:
            println("Hello, JenaTitaniumApp!")
            val turtleFile  = "person.ttl"
            val contextFile = "person-context.jsonld"


            // 1) Read Turtle
            val model: Model = ModelFactory.createDefaultModel()
            RDFDataMgr.read(model, turtleFile.getStream, Lang.TURTLE)

            // 2) Convert Model -> DatasetGraph -> RdfDataset
            val dsg        = DatasetGraphFactory.create(model.getGraph)
            val rdfDataset = JenaTitanium.convert(dsg)

            // 3) Read JSON-LD context from file
            val ctxStream: InputStream = contextFile.getStream
            val ctxJson                = Json.createReader(ctxStream).read()
            ctxStream.close()

            // 4) Expand RDF -> JSON-LD, enabling native integer
            val expanded = JsonLd
                .fromRdf(RdfDocument.of(rdfDataset))
                //.nativeTypes(true) // xsd:integer => JSON number
                .get()

            // 5) Compact with the loaded context
            val compacted = JsonLd
                .compact(JsonDocument.of(expanded), JsonDocument.of(ctxJson))
                //.options( {val opts = JsonLdOptions(); opts.setUseNativeTypes(true); opts } )
                .get()

            // 6) Create a pretty-printing JSON writer
            //    Using Jakarta JSON-P's PRETTY_PRINTING config
            val config = Map[String, AnyRef](
                JsonGenerator.PRETTY_PRINTING -> java.lang.Boolean.TRUE
            ).asJava

            val writerFactory = Json.createWriterFactory(config)
            val outputStream  = System.out

            val writerCompacted = writerFactory.createWriter(outputStream)
            val writerExpanded = writerFactory.createWriter(outputStream)

            writerExpanded.write(expanded)
            writerCompacted.writeObject(compacted.asJsonObject)

            writerExpanded.close()
            writerCompacted.close()






