import Colors._
import Implicits._
import SGF._
import scala.io.Source
import Utils._

/**
  * sbt "run -db -d path_to_sgf_dir"
  */
object Main {

  def main1(args: Array[String]) = {

    val sgfDir =
      try { Some(args(args.indexOf(args.find(_ == "-d").get) + 1)) }
      catch { case e: Exception => None }

    val color =
      try { Some(args.find(x => x == "-w" || x == "-b" || x == "-wb").get) }
      catch { case e: Exception => None }

          for {
            sgf <- sgfDir
            col <- color
          }

      // mode
    match {
      case Some(sgfDir) =>

        args.find{ x => x == "-db" || x == "-f" || x == "-gtp" || x == "-white" || x == "-black"} match {

          case Some(a) if a.toLowerCase == "-db" =>
            parseAllIn(sgfDir, Some(new DB))

          case Some(a) if a.toLowerCase == "-f" =>
            parseAllIn(sgfDir, Some(new Files))

          case Some(a) if a.toLowerCase == "-gtp" =>
            GTP_CmdHandler.listenAndServe()

          case None =>
            println("test mode (run mode was not given) ... ")
            parseAllIn(sgfDir, None, limit = Some(100))
        }

      case None =>
        throw new RuntimeException("Error: Specify sgf dir. (ex: sbt \"run -db -d sgf\"" )

    }
  }
  def main(args: Array[String]) = {

    { // sgf dir
      try { Some(args(args.indexOf(args.find(_ == "-d").get) + 1)) }
      catch { case e: Exception => None }
    }

    // mode
    match {
      case Some(sgfDir) =>

        args.find{ x => x == "-db" || x == "-f" || x == "-gtp" || x == "-white" || x == "-black"} match {

          case Some(a) if a.toLowerCase == "-db" =>
            parseAllIn(sgfDir, Some(new DB))

          case Some(a) if a.toLowerCase == "-f" =>
            parseAllIn(sgfDir, Some(new Files))

          case Some(a) if a.toLowerCase == "-gtp" =>
            GTP_CmdHandler.listenAndServe()

          case None =>
            println("test mode (run mode was not given) ... ")
            parseAllIn(sgfDir, None, limit = Some(100))
        }

      case None =>
        throw new RuntimeException("Error: Specify sgf dir. (ex: sbt \"run -db -d sgf\"" )

    }
  }

  def parseAllIn(dir: String, out: Option[OutputStorage], limit: Option[Int] = None) = {
    try {
      var count = 0
      listFilesIn(dir, limit, Some(".sgf")).par foreach { f =>
        count += 1; if (count % 1000 == 0) println(count)
        try {
          val res = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
          if (res.successful) for {
            r <- processParseResult(res.get)
            o <- out
          } commitResult(r, o)
        } catch {
          case e: java.nio.charset.MalformedInputException =>
            println("ignore strange file: " + f.getName)
        }
      }
    } finally out foreach { _.close() }
  }

  private def commitResult(res: (Seq[State], Seq[Move]), out: OutputStorage) = {
    val (states, moves) = res
    zipEach(states, moves){ (s, m) =>
      if (m.color == White) {
        out.commit(s.toChannels, m.pos, s.invalidChannel)
      }
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

}
