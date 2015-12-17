object InvalidPositionTest extends App {
  import Utils._
  import Implicits._
  import Colors._

  val board = createRandomBoard

  lazy val invalidChannel = {
    val dst = Array.range(0, Config.all) map { i =>
      // already occupied or suicide move
      // TODO: I am White only
      val cantPlay = board(i) != Empty || i.isSuicideMovePos(White, board)
      if (cantPlay) '1' else '0'
    }
    // ko (I don't suppose  now...)
    // if (invalidPosByKo != -1) dst(invalidPosByKo) = '1'
    dst
  }



  board.printState(19, 19, None, None)
  invalidChannel.printState(19, 19, None, None)
}
