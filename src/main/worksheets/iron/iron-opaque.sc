import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import io.circe.*
import io.circe.optics.JsonPath.*
import io.circe.parser.*
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given

object Data:
    opaque type Temperature <: String = String :| Pure
    object Temperature extends RefinedTypeOps.Transparent[Temperature]

    opaque type Name <: String = String
    object Name:
        def apply(value: String): Name           = value
        extension (name: Name) def value: String = name

end Data

import Data.*

val temp = Temperature("hello") // OK
temp.toUpperCase.asJson // OK
"2".asJson
temp.asJson

val name1 = Name("hello")
// OK
