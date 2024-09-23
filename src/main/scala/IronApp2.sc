



import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.generic.auto.*

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.circe.given

type Username = Alphanumeric DescribedAs "Username should be alphanumeric"

type Age = Positive DescribedAs "Age should be positive"

case class User(name: String :| Username, age: Int :| Age)

//Encoding
User("Iltotore", 8).asJson //{"name":"Iltotore", "age":18}

//Decoding
decode[User]("""{"name":"Iltotore","age":18}""") //Right(User(Iltotore, 18))



type FirstName = String :| Pure
object FirstName extends RefinedTypeOps[String, Pure, FirstName]

val firstName = FirstName("firstName")

summon[Encoder[FirstName]].apply(firstName) //Encodes as a String

case class Person(name: FirstName, age: Int)

Person(firstName, 42).asJson