
import io.github.iltotore.iron.*
//import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.constraint.any.Pure
import io.circe.*
import io.circe.optics.JsonPath.*
import io.circe.parser.*
import io.circe.syntax.*
import io.github.iltotore.iron.circe.given
import Data.Name.value

object Data:

  type FirstName = String :| Pure
  object FirstName extends RefinedTypeOps.Transparent[FirstName]


  opaque type Temperature <: String = String :| Pure
  object Temperature extends RefinedTypeOps.Transparent[Temperature]

  
  opaque type Name <: String = String
  object Name:
    def apply(value: String): Name = value
    extension (name: Name) def value: String = name

end Data


@main def runIronApp(): Unit =

  import Data.*

  val firstName = FirstName("firstName")

  //firstName.asJson

  val temp = Temperature("hello") // OK

  println(temp.value)
  println(temp.toUpperCase.asJson) // OK
  println("2".asJson)
  println(temp.asJson)

  val name = Name("hello")

  println(name.toUpperCase)

  println(Json.obj("name" -> temp.asJson))

  val aName:String = name