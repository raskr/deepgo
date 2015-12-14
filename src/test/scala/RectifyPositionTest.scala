object RectifyPositionTest extends App {

  import Implicits._

  assert(45.rectify(21, 19) == 1)
  assert(45.rectify(21, 19) == 21)

}
