// ok
object BoardTest extends App {
  import UtilMethods._
  import Implicits._

  val board = createRandomBoard

  board.printState(19, 19, None, None)

  board.toBoardChannel(true).toCharArray.printSelf(3, 361)
}
