import Utils._
import Colors._
import Implicits._

/**
 * board(3ch), ko(1ch), liberty(6ch), border(1ch), rank(9ch), lifeTime(1ch), invalid(1ch) -> 22ch
 *
 * @param board current board
 * @param invalidPosByKo position next player can't play
 */
case class State(board: Array[Char] = Array.fill(Constants.all)(Empty),
            invalidPosByKo: Int = -1,
            lifeSpans:Array[Int] = Array.fill(Constants.all)(0),
            rank: String) {

  lazy val invalidChannel = {
    val dst = zeros(Constants.all)
    (0 until Constants.all) foreach { i => if (board(i) != Empty) dst(i) = '1' }
    if (invalidPosByKo != -1) dst(invalidPosByKo) = '1'
    dst.mkString
  }

  def toChannels: String = {
    val a = board.toBoardChannel
    val b = board.toBorderChannel
    val c = board.toLibertyChannel
    val d = invalidPosByKo.toKoChannel
    val e = rank.toRankChannel
    val f = invalidChannel
    val g = lifeSpans.toLifespanChannel

//    assert(a.length == 361*3)
//    assert(b.length == 361*1)
//    assert(c.length == 361*6)
//    assert(d.length == 361*1)
//    assert(e.length == 361*9)
//    assert(f.length == 361*1)
//    assert(g.length == 361*1)

    new StringBuilder()
      .append(a)
      .append(b)
      .append(c)
      .append(d)
      .append(e)
      .append(f)
      .append(g)
      .toString()
  }

  /** concrete implementation is in Implicits */
  def createNextBy(move: Move): State = {
    // 1. update board
    val newBoard = board createNextBy move
    // 2. find `ko`
    val ko = board findKoBy move
    // 3. turns since
    val ls = lifeSpans.nextLifespans(board, newBoard)
    // return
    State(newBoard, ko, ls, rank)
  }

}
