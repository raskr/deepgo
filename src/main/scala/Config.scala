object Config {
  import Color._

  var DEBUG = false

  // ----------------------------------------
  // -------------- sgf parser --------------
  // ----------------------------------------
  var dia = 19
  def all = dia * dia
  def padDia = dia + 2
  def padAll = (dia + 2) * (dia + 2)

  var numPrevMoves = 0
  var numPred = 0

  // ---------------------------------
  // -------------- GTP --------------
  // ---------------------------------
  // rank
  val ownRank = "2d"
  val opponentRank = "1d"

  // color
  val ownColor = White
  val opponentColor = if (ownColor == White) Black else White

  // BoardSize command only can change Komi
  var Komi = 5.5

}
