

//import io.github.iltotore.iron.constraint.all.*

import io.circe.*

import io.circe.parser.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import io.circe.optics.JsonPath.*

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.circe.given

object Data:

  type FirstName = String :| Pure
  object FirstName extends RefinedTypeOps.Transparent[FirstName]


  opaque type Temperature <: String = String :| Pure
  object Temperature extends RefinedTypeOps[String, Pure, Temperature]

  
  opaque type Name = String
  object Name:
    def apply(value: String): Name = value
    extension (name: Name) def value: String = name


  opaque type TestName <: String = String
  object TestName:
      def apply(value: String): TestName = value

end Data


@main def runIronApp(): Unit =

  import Data.*


  val firstname0: FirstName = "John" // works because pure type alias
  val firstName = FirstName("firstName")

  //firstName.asJson // weird problem with iron-circe

  //val temp0: Temperature = "temperature1" // does not work because Temperature is opaque
  val temp = Temperature("temperature1") // OK
  println(temp.value) // just to prove that it generate value, but necessary to use the extension method
  println(temp.toUpperCase.asJson) // OK
  println("2".asJson)
  println(temp.asJson)
  println(Json.obj("temperature" -> temp.asJson))


  val name = Name("hello")
  println(name.value.toUpperCase) // because no subtype is defined, it is necessary to use the extension method
  //val aName:String = name // This will not compile because Name is not a subtype of String

  val testname: TestName = TestName("John")
  val testNameResult     = "Hello, " + testname // String concatenation works because FirstName <: String
  println(testNameResult) // Prints "Hello, John"