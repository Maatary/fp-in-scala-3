import monocle.{Focus, Lens}
import monocle.macros.GenLens
import monocle.syntax.all.*
import cats.effect.*
import cats.syntax.all.*



case class User(name: String, age: Int)

val user        = User("John", 40)


val userAgeLens1 = Lens[User, Int].apply(user => user.age)(age => user => user.copy(age = age))
val userAgeLens2: Lens[User, Int] = GenLens[User](_.age)
val userNameLens = Focus[User](_.name)
val userAgeLens3 = user.focus(_.age)

userAgeLens1.get(user) // 40
userAgeLens2.replace(50)(user) // User("John", 50)
userAgeLens3.replace(50)



userAgeLens2.modifyF(e => IO(e))(user)