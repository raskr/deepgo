import Color._
import Implicits._
import Config._

/**
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
    if (ansColor == White) rankW else rankB

  def invalidChannel: Array[Char] = {
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

  def legalChannel: String = {
    val dst = invalidChannel
    var i = 0
    while (i < all) {
      dst(i) = if (dst(i) == '1') '0' else '1'
      i += 1
    }
    dst.mkString
  }

  // 23ch
  def toChannels: Option[String] = ownRank map { rank =>
    new StringBuilder()
      .append(prevMovesChannel)       // 2
      .append(hist.toHistoryChannel)  // 1
      .append(board.toBoardChannel)   // 3
      .append(board.toBorderChannel)  // 1
      .append(koPos.toKoChannel)      // 1
      .append(rank.toRankChannel)     // 9
      .append(board.toLibertyChannel) // 6
      .toString()
  }

  def nextStateBy(moves: Array[Move]): State = {
    val move = moves.last
    val newBoard = if (!move.pass) board.createNextBoardBy(move) else board
    val ko = if (move.pass) board.findKoBy(move, newBoard) else -1
    val his = hist.nextHistory(board, newBoard)
    State(newBoard, his, ko, rankW, rankB, moves)
  }

}
