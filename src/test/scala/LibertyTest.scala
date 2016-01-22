// passed
object LibertyTest extends App {
  import Rules._
  import Utils._
  import Implicits._

  // init
  val board = createRandomBoard
  board.printState(19, 19, None, None)

  // result
  liberties(board).printState(19, 19)

  // channel test
  board.toLibertyChannel.toCharArray.printSelf(6, 361)

}
