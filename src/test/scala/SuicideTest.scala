object SuicideTest extends App {

  import Rules._
  import Utils._
  import Implicits._

  val init = createRandomBoard
  init.printState(19, 19, None, None)

  Array.range(0, 361).map { i =>
    val (x, y) = i.toCoordinate
    if (init(i) != Color.Empty) {
      false
    } else {
//      isSuicideMove(Move(Colors.Black, x, y), init) ||
        isSuicideMove(Move(Color.White, x, y, isValid=true), init)
    }
  }.map(x => if (x) 1 else 0).printState(19, 19)

}
