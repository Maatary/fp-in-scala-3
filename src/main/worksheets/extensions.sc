

import cats.implicits.given


extension (e: String)
    def asOption: Option[String] = if e.isEmpty then None else Some(e)




"".asOption