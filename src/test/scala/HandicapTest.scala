// ok
object HandicapTest extends App {
  import Implicits._
  for (i <- 2 to 9) {
    val a = Rules.genInitialBoard(Some(i))
    a.printState(19, 19, None, None)
  }
}
