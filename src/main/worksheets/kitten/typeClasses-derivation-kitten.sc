import cats.syntax.all.given
import cats.{Eq, Show}
import cats.derived.*


case class Person(name: String, age: Int) derives Show

Person("Alice", 42).show
