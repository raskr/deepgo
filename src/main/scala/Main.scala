import Implicits._
import SGF._
import Utils._
import Color._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * sbt "run -m db -c wb -d path_to_sgf_dir"
  */
object Main extends App {

  val res = Utils.parseArgs(args.toList)

  val dir = res.find{_._1 == "-d"}
  val color = res.find{_._1 == "-c"}
  val mode = res.find{_._1 == "-m"}
  val opponentRank = res.find{_._1 == "-o"}

  (dir, color, mode) match {
    case (Some(d), Some(c), Some(m)) if m._2 == "db" =>
      parseSGF(d._2, colorsFrom(c._2).map(new DB(_)))

    case (Some(d), Some(c), Some(m)) if m._2 == "f" =>
      parseSGF(d._2, colorsFrom(c._2).map(new Files(_)))

    case (_, _, Some(m)) if m._2 == "gtp" =>
      if (opponentRank.isEmpty) throw new RuntimeException("opponent rank is required")
      new GTP_CmdHandler(opponentRank.get._2).listenAndServe()

    case (Some(d), _, None) =>
      println("test mode (run mode was not given) ... ")
      parseSGF(d._2, Seq(), limit=Some(100))

    case _ => throw new RuntimeException("Illegal arguments")

  }

  def parseSGF(dir: String, outs: Seq[OutputStorage], limit: Option[Int] = None) = {
    try {
      listFilesIn(dir, limit, Some(".sgf")).par foreach { f =>
        try {
          val res = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
          if (res.successful)
            processParseResult(res.get, outs.map(_.color)) foreach { commitResult(_, outs) }
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
//      val i = states.indexOf(st)
//      val next = states(i).board.createNextBoardBy(mv)
//      if (next sameElements states(i+1).board) println("ok")
//      else println("ng")
      outs.foreach {out => if (out.color == mv.color) out.commit(st, mv) }
    }
  }

  /**
    * Main task. Convert case classes to Strings
    *
    * @param result result in 'one' file
    * @return (current states, targets wrt those)
    */
  def processParseResult(result: Collection, colors: Seq[Char]): Option[(Seq[State], Seq[Move])] = {
    result match {
      // result.successful is guaranteed by caller of this method
      case Collection(List(GameTree(Sequence(nodes: List[Node]), _))) =>
        if (nodes.isEmpty) return None

        var rankW: Option[String] = None
        var rankB: Option[String] = None
        // ============================================
        // Header of sgf
        // ============================================
        nodes.head.props foreach { prop: Property =>
          prop match {
            // rank (white player)
            case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String))))
              if a == "WR" => rankW = if (r.isStrong) Some(r) else None
            // rank (black player)
            case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String))))
              if a == "BR" => rankB = if (r.isStrong) Some(r) else None
            // other
            case _ =>
          }
        }

        // ============================================
        // moves
        // ============================================
        val dummyMv = Move('?', '?', '?', isValid=false)
        val states = ArrayBuffer(State(rankW=rankW, rankB=rankB, prevMove=dummyMv))
        val moves = ArrayBuffer(dummyMv)

        nodes.tail.foreach {
          _.props match {
            // the move
            case List(Property(PropIdent(col: String), List(PropValue(Point(a: Char, b: Char))))) =>
              val mv = Move(if (col.head.toLower == 'w') White else Black, a-'a', b-'a', isValid=true)
              if (mv.x <= 18 && mv.y <= 18) {
                states append states.last.nextStateBy(mv)
                moves append mv
              }
            // not a move
            case _ =>
          }
        }

        Some((states.init, moves.tail))
    }
  }

}
