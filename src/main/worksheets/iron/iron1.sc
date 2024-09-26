import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import io.circe.*
import io.circe.optics.JsonPath.*
import io.circe.parser.*
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given







type SingleItemList[A] = List[A] :| FixedLength[1]

val list: SingleItemList[Int] = List(42, 6).assume[FixedLength[1]]


list.asJson









