object StupidTest extends App {

  import Implicits._

//  val a = 41.rectify(1, 21, 19)
//  assert(a == 77)

  assert(Utils.num2coordinate(12) == (12, 0))
  assert(Utils.num2coordinate(0) == (0, 0))
  assert(Utils.num2coordinate(25) == (6, 1))
  assert(Utils.num2coordinate(40) == (2, 2))
}
