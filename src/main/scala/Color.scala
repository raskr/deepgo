object Color {
  val White = 'O'
  val Black = 'X'
  val Empty = '.'
  val Outside= 'V'

  def colorsFrom(str: String): Seq[Char] = {
    if (str == "w") Array(White)
    else if (str == "b") Array(Black)
    else if (str == "white") Array(White)
    else if (str == "black") Array(Black)
    else if (str == "wb") Array(White, Black)
    else if (str == "bw") Array(Black, White)
    else throw new RuntimeException
  }
}
