import Color._
import Implicits._
import Config._

/**
 * board(3ch), ko(1ch), liberty(6ch), border(1ch), groupSizes(2ch)
 * rank(9ch), lifeTime(1ch), invalid(1ch) -> 24ch
 *
 * @param board current board
 * @param koPos position next player can't play
 * @param prevMove move created this state
 */
case class State(board: Array[Char] = Array.fill(all)(Empty),
                 hist: Array[Int] = Array.fill(all)(0),
                 koPos: Int = -1,
                 rankW: Option[String],
                 rankB: Option[String],
                 prevMove: Move) {

  val ansColor = prevMove.color.opponent

  def myRank: Option[String] =
    if (prevMove.color.opponent == White) rankB else rankW

  def opponentRank: Option[String] =
    if (prevMove.color.opponent == White) rankW else rankB

  val invalidChannel = {
    val dst = Array.range(0, all) map { i =>
      // already occupied or suicide move
      val cantPlay = board(i) != Empty || i.isSuicideMovePos(ansColor, board)
      if (cantPlay) '1' else '0'
    }
    // ko
    if (koPos != -1) dst(koPos) = '1'
    dst.mkString
  }

  def toChannels: Option[String] =
    myRank map { rank =>
      new StringBuilder()
        .append(board.toBoardChannel)     // 3 tested
        .append(board.toBorderChannel)    // 1 tested
        .append(board.toLibertyChannel)   // 6 tested
        .append(koPos.toKoChannel)        // 1 tested
        .append(rank.toRankChannel)       // 9 tested
        .append(prevMove.toMoveChannel)   // 1 maybe ok
        .append(board.toGroupSizeChannel) // 2 tested
        .append(hist.toHistoryChannel)    // 1 tested
        .toString
    }

  def nextStateBy(move: Move): State = {
    val newBoard = board.createNextBoardBy(move)
    val ko = board.findKoBy(move, newBoard)
    val his = hist.nextHistory(board, newBoard)
    State(newBoard, his, ko, rankW, rankB, move)
  }

}
