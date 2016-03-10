import Color._
import Implicits._
import Config._

// `State` but immutable.
// Holds game states such as stone placement.
case class State(board: Array[Char],
                 hist: Array[Int] = Array.fill(all)(0),
                 koPos: Int = -1,
                 ansColor: Char,
                 rankW: Option[String],
                 rankB: Option[String]) {

  // This is used when GTP match to avoid illegal play
  def illegalPositionsChannel: Array[Char] = {
    val dst = Array.range(0, all) map { i =>
      // already occupied or suicide move
      val occupied = board(i) != Empty
      val cantPlay = occupied || i.isSuicideMovePos(ansColor, board)
      if (cantPlay) '1' else '0'
    }
    // ko
    if (koPos != -1) dst(koPos) = '1'
    dst
  }

  /**
    * @param color color of current player
    */
  def toChannels(color: Char): Option[String] = {
    val r = if (color == White) rankW else if (color == Black) rankB else None
    r map { rank =>
      new StringBuilder()
        .append(board.toBoardChannel(flip = color == Config.opponentColor))   // 3ch
        .append(board.toLibertyChannel(flip = color == Config.opponentColor)) // 6ch
        .append(hist.toHistoryChannel)                                        // 1ch
        .toString()
    }
  }

  /**
    * Create next board state by given move and game rules.
    *
    * @param move move played
    * @return next new state
    */
  def nextStateBy(move: Move): State = {
    val newBoard = board.createNextBoardBy(move)
    val ko       = board.findKoBy(move, newBoard)
    val his      = hist.nextHistory(board, newBoard)
    State(newBoard, his, ko, move.color.opponent, rankW, rankB)
  }

}
