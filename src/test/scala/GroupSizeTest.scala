// passed
object GroupSizeTest extends App {

  import Rules._
  import Utils._
  import Implicits._

  val board = createRandomBoard
  board.printState(19, 19, None, None)

  val sizes = groupSizes(board)
  sizes.map{x => ('0' + x).toChar}.printState(19, 19, None, None)

  // channel
  board.toGroupSizeChannel.toCharArray.printSelf(2, 361)

}