// passed
object GroupSizeTest extends App {

  import Rules._
  import Utils._
  import Implicits._

  val board = createRandomBoard
  println("current state")
  board.printState(19, 19, None, None)

  val sizes = groupSizes(board)
  println("sizes")
//  sizes.map{x => ('0' + x).toChar}.printState(19, 19, None, None)
  val a: Array[Float] = sizes.map{x => scala.math.exp(0.01*x).toFloat-1}
  println("aaaaaaaaaaaaaaaaaaaaaaaaaa")
  a.printSelf(19, 19)

  // channel
  println("channel")
  board.toGroupSizeChannel.toCharArray.printSelf(2, 361)

}