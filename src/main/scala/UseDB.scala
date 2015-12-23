import Colors._
import Implicits._
import SGF._
import scala.io.Source
import Utils._

/**
  * sbt "run create path_to_sgf_dir"
  * sbt "run test path_to_sgf_dir"
  */
object UseDB extends App {

  override def main(args: Array[String]) = {

    // parse argument
    args.toList match {

      // create data for nn
      case head :: sgfDir :: _ if head == "create" =>
        parseAllIn(sgfDir, Some(new DB), limit=None)

      // test with little sgf data (default is 100)
      case head :: sgfDir :: _ if head == "test" =>
        parseAllIn(sgfDir, db=None, limit=Some(100))

      // auto play with gtp
      case head :: _ if head == "gtp" =>
        GTP_CmdHandler.listenAndServe()

      // error
      case _ => throw new RuntimeException("ex) sbt \"run test path_to_sgf_dir\"")
    }

  }

  /**
    * Main task. Convert case classes to Strings
    *
    * @param result result in 'one' file
    * @return (current states, targets wrt those)
    */
  def processParseResult(result: Collection): Option[(List[State], List[Move])] = {
    result match {
      // result.successful is guaranteed by caller of this method
      case Collection(List(GameTree(Sequence(nodes: List[Node]), _))) =>
        if (nodes.isEmpty) return None

        var rank: Option[String] = None
        // ============================================
        // Header of sgf file
        // ============================================
        nodes.head.props foreach { prop: Property =>
          prop match {
            // rank (white player)
            case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String))))
              if a == "WR" =>
              rank = Some(r)
            // other
            case _ =>
          }
        }
        // ============================================
        // header end
        // ============================================

        rank match {
          case Some(rnk) if rnk.isStrong =>
            // ============================================
            // fold moves
            // ============================================
            val dummyMove = Move('?', '?', '?', isValid = false)
            val ret = nodes.tail.foldLeft(List(State(rank=rnk, move=dummyMove)), List(dummyMove)) {
              case ((states, moves), node) =>
                node.props match {
                  // the move
                  case List(Property(PropIdent(col: String), List(PropValue(Point(a: Char, b: Char))))) =>
                    val mv = Move(if (col.head == 'W') White else Black, a - 'a', b - 'a', isValid=true)
                    if (mv.x >= 18 || mv.y >= 18) { // invalid move
                      (states, moves)
                    } else {
                      (states.head.nextStateBy(mv) :: states, mv :: moves)
                    }
                  // not a move
                  case _ => (states, moves)
                }
            }
            // ============================================
            // folding end
            // ============================================
            Some((ret._1, ret._2))
          case _ => None
        }
    }
  }

  def commitResult(res: (Seq[State], Seq[Move]), db: DB) = {
      zipEach(res._1.init, res._2.tail) { (s, m) =>
        if (m.color == White) db.insert(s, m)
      }
  }

  def parseAllIn(dir: String, db: Option[DB], limit: Option[Int]) = {
    if (db.isEmpty) println("run in test mode")
    try {
      listFilesIn(dir, limit, extension = Some(".sgf")).par foreach { f =>
        try {
          val res = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
          if (res.successful) {
            for {
              a <- processParseResult(res.get)
              b <- db
            } yield commitResult(a, b)
          }
        } catch {
          case e: java.nio.charset.MalformedInputException =>
            println("ignore strange file: " + f.getName)
        }
      }
      db foreach { a => a.save(); println("saving done") }
    } finally db foreach { _.close() }

  }
}
