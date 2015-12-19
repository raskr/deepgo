object StupidTest extends App {

  import Implicits._

//  val a = 41.rectify(1, 21, 19)
//  assert(a == 77)

  assert(12.toCoordinate == (12, 0))
  assert(0.toCoordinate == (0, 0))
  assert(25.toCoordinate == (6, 1))
  assert(40.toCoordinate == (2, 2))
}
