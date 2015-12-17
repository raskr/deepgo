// ok
object BorderTest extends App {

  import Implicits._

  val dia = 19
  val a = Array.fill(dia*dia)(0)
  Utils.borderPositions(dia).foreach{ a(_) = 1 }
  a.printState(dia, dia)

  // channel
  val b = Utils.createRandomBoard
  b.printState(19, 19, None, None)
  b.toBorderChannel.toCharArray.printState(19, 19, None, None)

}
