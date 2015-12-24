import Colors._
import Implicits._
import SGF._
import scala.io.Source
import Utils._

/**
  * sbt "run path_to_sgf_dir create"
  * sbt "run  path_to_sgf_dir test"
  */
object UseDB {

  def main(args: Array[String]) = try {

    // « can throw exception
    val sgfDir = args(args.indexOf(args.find(_ == "-d")) + 1)

    // mode
    args.find{x => x == "-db" || x == "-f" || x == "-gtp"} match {

      case Some(a) if a == "-db" =>
        parseAllIn(sgfDir, Some(new DB))

      case Some(a) if a ==  "-f" =>
        parseAllIn(sgfDir, Some(new Files))

      case Some(a) if a ==  "-gtp" =>
        GTP_CmdHandler.listenAndServe()

      case None =>
        println("test mode. (run mode didn't specified.)")
        parseAllIn(sgfDir, None, limit=Some(100))
    }

  } catch {
    case e: IndexOutOfBoundsException =>
      println(e)
      throw new RuntimeException("Error! Sgf directory may not be Specified.")
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
            val dummyMove = Move('?', '?', '?', isValid=false)
            val ret = nodes.tail.foldLeft(List(State(rank=rnk, prevMove=dummyMove)), List(dummyMove)) {
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
            Some((ret._1.tail, ret._2.init))
          case _ => None
        }
    }
  }

  def commitResult(res: (Seq[State], Seq[Move]), out: OutputStorage) = {
    val (states, moves) = res
    zipEach(states, moves){ (s, m) =>
      if (m.color == White) {
        out.commit(s.toChannels, m.pos, s.invalidChannel)
      }
    }
  }

  def parseAllIn(dir: String, out: Option[OutputStorage], limit: Option[Int] = None) = {
    try {
      listFilesIn(dir, limit, extension = Some(".sgf")).par foreach { f =>
        try {
          val res = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
          if (res.successful) {
            for {
              a <- processParseResult(res.get)
              b <- out
            } yield commitResult(a, b)
          }
        } catch {
          case e: java.nio.charset.MalformedInputException =>
            println("ignore strange file: " + f.getName)
        }
      }
    } finally out foreach { _.close() }

  }
}
