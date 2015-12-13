import scala.util.Random

object LifeSpanTest extends App {

  import Implicits._
  import Colors.White

  val rand = new Random
  val prevLifeSpans: Array[Int] = Array.fill(Constants.all)(rand.nextInt(9))
  val prevBoard = Utils.createRandomBoard
  val curBoard = prevBoard.createNextBy (Move(White, 'a', 'a'))

  // before
  prevBoard.printSelf(Constants.dia, Constants.dia)
  prevLifeSpans.toLifespanChannel.toCharArray.printSelf(Constants.dia, Constants.dia)

  // update
  val newLifeSpans = prevLifeSpans.nextLifespans(prevBoard, curBoard)

  // after
  curBoard.printSelf(Constants.dia, Constants.dia)
  newLifeSpans.toLifespanChannel.toCharArray.printSelf(Constants.dia, Constants.dia)

}
