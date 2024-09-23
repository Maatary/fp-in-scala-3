import ciris.*
import cats.syntax.all.*
import cats.effect.*
import cats.effect.unsafe.implicits.global


val okConf = ConfigValue.default(2)
val fault1 = env("fault1").as[String]
val fault2 = env("fault2").as[String]


val envConfig = (fault1,fault2).parMapN(_ -> _)

envConfig.attempt[IO].unsafeRunSync()




