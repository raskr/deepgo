import scala.collection.mutable.ArrayBuffer

object ChannelTest {
  def apply(arg: String) = {
    var isBoard = true

    val len = arg.length
    val n_ch = len / 361
    val b = ArrayBuffer[String]()
    var remain = arg

    for (i <- 1 until n_ch) {
      b.append(remain.take(361))
      remain = remain.takeRight(len - i * 361)
    }

    import Implicits._

    b.foreach {x =>
      if (isBoard) {
        println("board")
        isBoard = false
      } else {
        println("prev")
        isBoard = true
      }
      x.toCharArray.printState(19, 19, None, None)
    }
  }
}
