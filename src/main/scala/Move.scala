// top-left is origin
case class Move(color: Char, x: Int, y: Int, isValid: Boolean) {

  val pos = y * 19 + x

  def toMoveChannel = {
    val a = Array.fill(Config.all)('0')
    if (isValid) a(pos) = '1'
    a
  }
}
