object UseFile extends App {

  import Colors._
  import Implicits._
  import SGF._
  import scala.io.Source
  import Utils._
  
  override def main(args: Array[String]) = {

    // parse argument
    args.toList match {

      // create data for nn
      case head :: sgfDir :: _ if head == "create" =>
        parseAllIn(sgfDir, Some(new FileManager), limit=None)

      // test with little sgf data (default is 100)
      case head :: sgfDir :: _ if head == "test" =>
        parseAllIn(sgfDir, fm=None, limit=Some(100))

      // auto play with gtp
      case head :: _ if head == "gtp" =>
        GTP_CmdHandler.listenAndServe()

      // error
      case _ => throw new RuntimeException("do like `sbt \"run test path_to_sgf_dir\"`")
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
            // ===========================================x=
            val ret = nodes.tail.foldLeft(List(State(rank=rnk)), List(Move('?', '?', '?'))) {
              case ((states, moves), node) =>
                node.props match {
                  // the move
                  case List(Property(PropIdent(col: String), List(PropValue(Point(a: Char, b: Char))))) =>
                    val mv = Move(if (col.head == 'W') White else Black, a - 'a', b - 'a')
                    if (mv.isInvalid) (states, moves)
                    else (states.head.nextStateBy(mv) :: states, mv :: moves)
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

  def parseAllIn(dir: String, fm: Option[FileManager], limit: Option[Int]) = {
    if (fm.isEmpty) println("run in test mode")
    val files = listFilesIn(dir, limit, extension = Some(".sgf")).par
    var (all, current) = (files.size, 0)
    files foreach { f =>
      current += 1
      if (current % 1000 == 0) println(current / all)
      println(current)
      val res = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
      if (res.successful) {
        val pRes = processParseResult(res.get)
        pRes foreach { case (states, moves) =>
          zipEach(states, moves) { (state, move) =>
            if (move.color == White) {
              if (fm.isEmpty) state.toChannels
              else fm foreach { _ commit (state.toChannels, move.pos, state.invalidChannel) }
            }
          }
        }
      }
    }
  }

}
