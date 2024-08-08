
import cats.effect.IOApp
import cats.effect.IO
import fs2.io.file.*
import fs2.*
import fs2.data.csv.*
import fs2.data.csv.generic.*
import fs2.data.csv.generic.semiauto.*

object ParseGeneticVariant extends IOApp.Simple {

    case class Test(i: Int, s: String)

    given RowDecoder[Test] = deriveRowDecoder[Test]





    case class GeneticVariant(termite_id: String, snpId: Long, 
                              nucleotide_change: List[String],
                              gene_info: List[String], 
                              variant_type: String, 
                              clinical_significance: List[String],
                              molecular_consequence: List[String])

    val toGeneticVariant = (line: String) => {
        val columns = line.split("\t")
        GeneticVariant(
            columns(0),
            columns(1).toLong,
            columns(2).split(",").filter(_.nonEmpty).toList,
            columns(3).split(",").filter(_.nonEmpty).toList,
            columns(4),
            columns(5).split(",").filter(_.nonEmpty).toList,
            columns(6).split(",").filter(_.nonEmpty).toList
        )
    }

    //given RowDecoder[Row] = deriveRowDecoder[Row]


    override def run: IO[Unit] = {

        val filePath = getClass.getResource("20.tsv").getPath

        Files[IO].readAll(Path(filePath), 10 , Flags.Read)
          .through(text.utf8.decode)
          .through(text.lines)
          .filterNot(s => s.isBlank() || s.isEmpty())
          .chunkN(5)
          .prefetch
          .map {_.toList map toGeneticVariant}
          .map{Chunk.from(_)}.unchunks
          .evalMap(IO.println(_))
          .compile.drain


    }
  
}
