import cats.effect.*
import cats.syntax.all.*
import monocle.syntax.all.*
import monocle.macros.GenLens
import monocle.*





object MonocleApp extends IOApp.Simple {

    override def run: IO[Unit] = {

        case class User(name: String, age: Int)

        val user                             = User("John", 40)
        val userAgeLens : Lens[User, Int]    = Lens[User, Int](_.age)(age => user => user.copy(age = age))
        val userNameLens: Lens[User, String] = Focus[User](_.name)
        val userAgeLens2: Lens[User, Int]    = GenLens[User](_.age)

        user.focus(_.age).replace(50)
        userAgeLens.replace(20)
        userAgeLens.modifyF(e => IO(e))(user) flatMap(IO.println(_))
}
}
