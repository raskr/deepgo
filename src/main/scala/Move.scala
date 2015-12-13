case class Move(color: Char, a: Char, b: Char) {
  val (x, y) = (a - 97, b - 97)
  val pos = y * 19 + x
  // This method is needed because sgf file sometimes contains
  // strange board such as over 19 * 19.
  val isInvalid = !(x <= 18 && y <= 18)
}
