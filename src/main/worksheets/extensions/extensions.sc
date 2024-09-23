
import cats.Show
import cats.syntax.all.given


object StringOps:
    extension (e: String)
        def asOption: Option[String] = if e.isEmpty then None else Some(e)


import StringOps.*

"hello".asOption


//Basically a given can be named or anonymous.
// If it is named, it can be imported and used in other parts of the code.
// If it is anonymous, it can only be used in the scope where it is defined.

given Show[Int] with
    def show(i: Int): String = s"Int: $i"

given Show[Int] = new Show[Int]:
    def show(i: Int) = s"Int: $i"

given Show[Int] = {
    i => s"Int: $i"
}


given myShow[T]: Show[T] with
    println("myShow initialized")
    def show(t: T): String = s"myShow: $t"

/*given otherShow[T]: Show[T] = new Show[T]:
    println("otherShow initialized")
    def show(t: T): String = s"otherShow: $t"*/

/*given [T]: Show[T] = new Show[T]:
    def show(t: T): String = s"Show: $t"*/


"2".show
"3".show


val defaultInt = 422
given Int = 42
given Int = defaultInt

given anInt: Int = 42
given anotherInt: Int = defaultInt


import scala.language.implicitConversions

case class Dollars(amount: Double)
case class Percentage(value: Double)
case class Salary(gross: Double, deductions: Percentage):
    def net: Dollars = Dollars(gross * (1 - deductions.value / 100))

given Conversion[Double,Dollars] = d => Dollars(d)
given Conversion[Double,Percentage] = d => Percentage(d)

val salary = Salary(100_000.0, 20.0)
println(s"salary: $salary. Net pay: ${salary.net}")

given Conversion[Int,Dollars] with
    def apply(i:Int): Dollars= Dollars(i.toDouble)

val dollars: Dollars = 10

println(s"Dollars created from an Int: $dollars")





