import Colors._
import Implicits._

/**
 * board(3ch), ko(1ch), liberty(6ch), border(1ch), groupSizes(2ch)
 * rank(9ch), lifeTime(1ch), invalid(1ch) -> 24ch
 *
 * @param board current board
 * @param koPos position next player can't play
 * @param move move that created this state
 */
case class State(board: Array[Char] = Array.fill(Config.all)(Empty),
                 hist: Array[Int] = Array.fill(Config.all)(0),
                 koPos: Int = -1,
                 rank: String,
                 move: Move) {

  val invalidChannel = {
    val dst = Array.range(0, Config.all) map { i =>
      // already occupied or suicide move
      // TODO: I am White only
      val cantPlay = board(i) != Empty || i.isSuicideMovePos(White, board)
      if (cantPlay) '1' else '0'
    }
    // ko
    if (koPos != -1) dst(koPos) = '1'
    dst.mkString
  }

  def toChannels: String = new StringBuilder()
    .append(board.toBoardChannel)       // tested
    .append(board.toBorderChannel)      // tested
    .append(board.toLibertyChannel)     // tested
    .append(board.toGroupSizeChannel)   // tested
    .append(koPos.toKoChannel)          // tested
    .append(rank.toRankChannel)         // tested
    .append(move.toMoveChannel)         // maybe ok
    .append(hist.toHistoryChannel)      // tested
    .toString()

  def nextStateBy(move: Move): State = {
    // 1. update board
    val newBoard = board.createNextBoardBy(move)
    // 2. find `ko`
    val ko = board.findKoBy(move, newBoard)
    // 3. turns since
    val ls = hist.nextLifespans(board, newBoard)
    // return
    State(newBoard, ls, ko, rank, Some(move))
  }

}
