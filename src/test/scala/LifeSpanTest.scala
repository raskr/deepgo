import scala.util.Random

object LifeSpanTest extends App {

  import Implicits._
  import Colors.White

  val rand = new Random
  val prevLifeSpans: Array[Int] = Array.fill(Config.all)(rand.nextInt(9))
  val prevBoard = Utils.createRandomBoard
  val curBoard = prevBoard.createNextBoardBy(Move(White, 'a', 'a'))

  // before
  prevBoard.printSelf(Config.dia, Config.dia)
  prevLifeSpans.toLifespanChannel.toCharArray.printSelf(Config.dia, Config.dia)

  // update
  val newLifeSpans = prevLifeSpans.nextLifespans(prevBoard, curBoard)

  // after
  curBoard.printSelf(Config.dia, Config.dia)
  newLifeSpans.toLifespanChannel.toCharArray.printSelf(Config.dia, Config.dia)

}
