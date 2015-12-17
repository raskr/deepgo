// ok. invalid rank is validated before call toRankChannel()
object RankTest extends App {

  import Implicits._

  //"0d".toRankChannel.toCharArray.printSelf(9, 361)
  "1d".toRankChannel.toCharArray.printSelf(9, 361)

}
