import Color._
import Implicits._
import Config._

/**
 * board(3ch), ko(1ch), liberty(6ch), border(1ch), groupSizes(2ch)
 * rank(9ch), lifeTime(1ch), invalid(1ch) -> 24ch
 *
 * @param board current board
 * @param koPos position next player can't play
 * @param prevMoves
 */
case class State(board: Array[Char],
                 hist: Array[Int] = Array.fill(all)(0),
                 koPos: Int = -1,
                 rankW: Option[String],
                 rankB: Option[String],
                 prevMoves: Array[Move]) {

  def prevMovesChannel: String = {
    if (Config.numPrevMoves == 0) return ""
    val size = prevMoves.length

    if (size == Config.numPrevMoves) {
      prevMoves.map { move =>
        val b = Utils.zeros(Config.all)
        if (move.isValid) b(move.pos) = '1'
        b
      }.reduce(Array.concat(_, _)).mkString
    }
    else if (size > Config.numPrevMoves ){
      prevMoves.take(Config.numPrevMoves).map { move =>
        val b = Utils.zeros(Config.all)
        if (move.isValid) b(move.pos) = '1'
        b
      }.reduce(Array.concat(_, _)).mkString
    }
    else { // few moves
      val dst = prevMoves.map { move =>
        val b = Utils.zeros(Config.all)
        if (move.isValid) b(move.pos) = '1'
        b
      }.reduce(Array.concat(_, _))

      Array.concat(dst, Utils.zeros((Config.numPrevMoves - size) * Config.all)).mkString
    }
  }

  val prevMove = prevMoves.last
  val ansColor = prevMove.color.opponent

  def ownRank: Option[String] =
    if (prevMove.color.opponent == White) rankB else rankW

  def opponentRank: Option[String] =
    if (prevMove.color.opponent == White) rankW else rankB

  def invalidChannel = {
    val dst = Array.range(0, all) map { i =>
      // already occupied or suicide move
      val cantPlay = board(i) != Empty || i.isSuicideMovePos(ansColor, board)
      if (cantPlay) '1' else '0'
    }
    // ko
    if (koPos != -1) dst(koPos) = '1'
    dst
  }

  def legalChannel = {
    var i = 0
    val dst = invalidChannel
    while (i < all) {
      dst(i) = if (dst(i) == '1') 0 else 1
      i += 1
    }
    dst.mkString
  }

  // for `liberty'. Not for future moves
  //var nextBoard: Option[Array[Char]] = None

  def toChannels: Option[String] = for {
    rank <- ownRank
  } yield new StringBuilder()
    .append(hist.toHistoryChannel) // 1 tested

    .append(board.toLibertyChannel) // 6 tested
    .append(board.toBoardChannel) // 3 tested
    .append(board.toBorderChannel) // 1 tested
    .append(koPos.toKoChannel) // 1 tested
    .append(rank.toRankChannel) // 9 tested
    .append(legalChannel) // 1 maybe ok

    .append(prevMovesChannel) // n maybe ok
    .toString()


  def nextStateBy(moves: Array[Move]): State = {
    val move = moves.last
    val newBoard = board.createNextBoardBy(move)
    //nextBoard = Some(newBoard)
    val ko = board.findKoBy(move, newBoard)
    val his = hist.nextHistory(board, newBoard)
    State(newBoard, his, ko, rankW, rankB, moves)
  }

}
