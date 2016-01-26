// ok
object InvalidChannelTest extends App {
  import Utils._
  import Implicits._

  val board = createRandomBoard
  board.printState(19, 19, None, None)
  val s = State(board, rankB = None, rankW = None, prevMoves = Array(Move(Color.White, 1, 1, true)))

  s.invalidChannel.printState(19, 19, None, None)
}
