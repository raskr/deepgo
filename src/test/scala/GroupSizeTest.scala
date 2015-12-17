// passed
object GroupSizeTest extends App {

  import Rules._
  import Utils._
  import Implicits._

  val board = createRandomBoard
  board.printState(19, 19, None, None)

  val sizes = groupSizes(board)
  sizes.printState(19, 19)

}
