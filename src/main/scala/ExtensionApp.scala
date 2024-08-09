
/*object TypeClass:

    trait MyShow[T]:
        extension (t: T)
            def show: String

    given aShow[T]: MyShow[T] with
        extension (t: T)
            def show: String =  s"myShow: $t"

end TypeClass*/


object ExtensionApp:

    def main(args: Array[String]): Unit =

        object StringOps:
            extension (e: String)
                def asOption: Option[String] = if e.isEmpty then None else Some(e)

        import StringOps.*

        println("hello".asOption)


        trait MyShow[T]:
            println("MyShow initialized")
            extension (t: T) def show: String

        given aShow[T]: MyShow[T] with
            extension (t: T) def show: String =  s"showing: ${t.toString}"

        object MyShow:
            def apply[T](using instance: MyShow[T]): MyShow[T] = instance

        println(MyShow.show(0))
        println(summon[MyShow[Int]].show(1))
        println("2".show)
        println("3".show)

    end main



end ExtensionApp





