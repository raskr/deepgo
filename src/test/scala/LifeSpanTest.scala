import scala.util.Random

// ok maybe ...
object LifespanTest extends App {

  import Implicits._
  import Colors.White

  // -------------------- prepare
  val rand = new Random

  // random, so wrong lifespan but i don't mind that
  val prevLifeSpans: Array[Int] = Array.fill(Config.all)(rand.nextInt(9))
  val prevBoard = Utils.createRandomBoard

  val curBoard = prevBoard.clone()
  // mod the board 30 times randomly
  (0 until 30) foreach { x =>
    val rnd = rand.nextInt(360)
    curBoard(rnd) = if (rnd > 180) Colors.White else Colors.Black
  }
  // -------------------- prepare end


  // before
  prevBoard.printState(Config.dia, Config.dia, None, None)
  prevLifeSpans.toLifespanChannel.toCharArray.printSelf(Config.dia, Config.dia)

  // update
  val newLifeSpans = prevLifeSpans.nextLifespans(prevBoard, curBoard)

  // after
  curBoard.printState(Config.dia, Config.dia, None, None)
  newLifeSpans.toLifespanChannel.toCharArray.printSelf(Config.dia, Config.dia)

}
