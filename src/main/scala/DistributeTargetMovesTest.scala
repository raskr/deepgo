import Implicits._
import Utils._

object DistributeTargetMovesTest {

  /**
    * @param dst return value of Main.distributeTargetMoves()
    */
  def apply(dst: Seq[(State, Seq[Move])]) = {
    val (dstSt, dstMv) = dst.unzip
    zipEach(dstSt, dstMv) { (st, ms: Seq[Move]) =>
      var answerState = st
      var createdState = st

      ms.foreach{ m =>
        try {
          createdState = createdState.nextStateBy(m)
          answerState = dstSt.nextOf(answerState)
          assert(createdState.board sameElements answerState.board)
        } catch { case e: IndexOutOfBoundsException => /* ignore */ }
      }
    }
  }

}
