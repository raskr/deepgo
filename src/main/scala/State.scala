import Utils._
import Colors._
import Implicits._

/**
 * board(3ch), ko(1ch), liberty(6ch), border(1ch), groupSizes(2ch)
  * rank(9ch), lifeTime(1ch), invalid(1ch) -> 24ch
 *
 * @param board current board
 * @param invalidPosByKo position next player can't play
 */
case class State(board: Array[Color] = Array.fill(Constants.all)(Empty),
                 lifeSpans: Array[Int] = Array.fill(Constants.all)(0),
                 invalidPosByKo: Int = -1, rank: String) {

  lazy val invalidChannel = {
    val dst = zeros(Constants.all)
    (0 until Constants.all) foreach { i => if (board(i) != Empty) dst(i) = '1' }
    if (invalidPosByKo != -1) dst(invalidPosByKo) = '1'
    dst.mkString
  }

  def toChannels: String = new StringBuilder()
    .append(board.toBoardChannel)
    .append(board.toBorderChannel)
    .append(board.toLibertyChannel)
    .append(board.toGroupSizeChannel)
    .append(invalidPosByKo.toKoChannel)
    .append(rank.toRankChannel)
    .append(invalidChannel)
    .append(lifeSpans.toLifespanChannel)
    .toString()

  // concrete implementation is in Rules.scala
  def createNextBy(move: Move): State = {
    // 1. update board
    val newBoard = board createNextBoardBy move
    // 2. find `ko`
    val ko = board findKoBy move
    // 3. turns since
    val ls = lifeSpans.nextLifespans(board, newBoard)
    // return
    State(newBoard, ls, ko, rank)
  }

}
