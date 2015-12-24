import Implicits._
import SGF._
import scala.io.Source
import Utils._
import Color._

/**
  * example ...
  * sbt "run -db -wb -d path_to_sgf_dir"
  */
object Main {

  def main(args: Array[String]) = {

    val sgfDir =
      try { Some(args(args.indexOf(args.find(_ == "-d").get) + 1)) }
      catch { case e: Exception => None }

    val color = args.find(x => x == "-w" || x == "-b" || x == "-wb")

    (sgfDir, color) match {
      case (Some(sgf), Some(col)) =>

        args.map(_.tail).find{ x => x == "db" || x == "f" || x == "gtp"} match {

          case Some(mode) if mode.toLowerCase == "db" =>
            parseAllIn(sgf, colorsFrom(col.tail).map(new DB(_)))

          case Some(mode) if mode.toLowerCase == "f" =>
            parseAllIn(sgf, colorsFrom(col.tail).map(new Files(_)))

          case Some(mode) if mode.toLowerCase == "gtp" =>
            new GTP_CmdHandler().listenAndServe()

          case None =>
            println("test mode (run mode was not given) ... ")
            parseAllIn(sgf, Seq(), limit=Some(100))
        }

      case _ =>
        println("test mode (run mode was not given) ... ")
    }

  }

  def parseAllIn(dir: String, outs: Seq[OutputStorage], limit: Option[Int] = None) = {
    try {
      var count = 0
      listFilesIn(dir, limit, Some(".sgf")).par foreach { f =>
        count += 1; if (count % 1000 == 0) println(count)
        try {
          val res = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
          if (res.successful)
            processParseResult(res.get) foreach { commitResult(_, outs) }
        } catch {
          case e: java.nio.charset.MalformedInputException =>
            println("ignore strange file: " + f.getName)
        }
      }
    } finally outs foreach { _.close() }
  }

  private def commitResult(res: (Seq[State], Seq[Move]), outs: Seq[OutputStorage]) = {
    val (states, moves) = res
    zipEach(states, moves){ (st, mv) =>
      outs.foreach { out => if (out.color == mv.color) out.commit(st, mv) }
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
