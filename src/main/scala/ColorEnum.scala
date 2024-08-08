
import Color.*

enum ColorEnum:
  case Red, Green, Blue
  def isRed: Boolean = this == Red


val x = ColorEnum.valueOf("Red") // ColorEnum.Red

enum Color(val rgb: Int):
    case Red   extends Color(0xFF0000)
    case Green extends Color(0x00FF00)
    case Blue  extends Color(0x0000FF)
    case Mix(override val rgb: Int) extends Color(rgb)

val y = Mix(0x123456).rgb // 1193046