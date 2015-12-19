// top-left is origin
case class Move(color: Char, x: Int, y: Int) {
  val pos = y * 19 + x
  // This method is needed because sgf file sometimes contains
  // strange board such as over 19 * 19.
  val isInvalid = !(x <= 18 && y <= 18)
}
