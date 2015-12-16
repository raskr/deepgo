object GroupSizeTest extends App {

  import Rules._
  import Utils._
  import Implicits._

  val board = createRandomBoard
  board.printSelf(19, 19)

  val sizes = groupSizes(board)
  sizes.printState(19, 19)

}
