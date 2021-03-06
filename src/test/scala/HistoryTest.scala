import scala.util.Random

// ok maybe ...
object HistoryTest extends App {

  import Implicits._

  // -------------------- prepare
  val rand = new Random

  // random, so wrong lifespan but i don't mind that
  val prevLifeSpans: Array[Int] = Array.fill(Config.all)(rand.nextInt(9))
  val prevBoard = UtilMethods.createRandomBoard

  val curBoard = prevBoard.clone()
  // mod the board 30 times randomly
  (0 until 30) foreach { x =>
    val rnd = rand.nextInt(360)
    curBoard(rnd) = if (rnd > 180) Color.White else Color.Black
  }
  // -------------------- prepare end


  // before
  prevBoard.printState(Config.dia, Config.dia, None, None)
  prevLifeSpans.toHistoryChannel.toCharArray.printSelf(Config.dia, Config.dia)

  // update
  val newLifeSpans = prevLifeSpans.nextHistory(prevBoard, curBoard)

  // after
  curBoard.printState(Config.dia, Config.dia, None, None)
  newLifeSpans.toHistoryChannel.toCharArray.printSelf(Config.dia, Config.dia)

}
