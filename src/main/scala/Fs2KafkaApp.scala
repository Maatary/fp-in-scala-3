
import cats.effect.{IO, IOApp}
import fs2.io.file.*
import fs2.kafka.*
import fs2.text




object Fs2KafkaApp extends IOApp.Simple:

    def run: IO[Unit] =

        val producerSettings = TransactionalProducerSettings(
            s"transactional-id",
            ProducerSettings[IO, String, String]
              .withBootstrapServers("localhost:9092")
              .withRetries(10)
            )


        TransactionalKafkaProducer
          .stream(producerSettings)
          .flatMap { producer =>
              Files[IO].readAll(Path(getClass.getResource("PSrelationsDetailed_pairs.csv").getPath))
                .through(text.utf8.decode)
                .through(text.lines)
                .chunkN(100)
                .map {_.toList}
                .map {_.map { s => ProducerRecord("test-fs2-kafka", "key", s) }}
                .map { records => ProducerRecords(records) }
                .evalTap{records => IO.println( s"Producing batch of ${records.toString} records")}
                .evalMap(records => producer.produceWithoutOffsets(records))
          }
          .compile.drain

        /*Files[IO].readAll(Path(getClass.getResource("PSrelationsDetailed_pairs.csv").getPath))
          .through(text.utf8.decode)
          .through(text.lines)
          .chunkN(100)
          .map {_.toList}
          .map {_.map { s => ProducerRecord("topic", "key", s) }}
          .map { records => ProducerRecords(records) }
          .evalTap { records => IO.println(s"Producing batch of ${records.toString} records") }
          .compile.drain*/


