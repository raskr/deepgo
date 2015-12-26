import Implicits._
import SGF._
import Utils._
import Color._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * example ...
  * sbt "run -m db -c wb -d path_to_sgf_dir"
  */
object Main extends App {

  case class Arg(pref: String, v: String)
  type Args = mutable.Set[Arg]

  val res = parseArgs(args.toList)

  (res.find{_.pref == "-d"}, res.find{_.pref == "-c"}, res.find{_.pref == "-m"}) match {
    case (Some(d), Some(c), Some(m)) if m.v == "db" =>
      parseSGF(d.v, colorsFrom(c.v).map(new DB(_)))

    case (Some(d), Some(c), Some(m)) if m.v == "f" =>
      parseSGF(d.v, colorsFrom(c.v).map(new Files(_)))

    case (_, _, Some(m)) if m.v == "gtp" =>
      new GTP_CmdHandler().listenAndServe()

    case (Some(d), _, None) =>
      println("test mode (run mode was not given) ... ")
      parseSGF(d.v, Seq(), limit=Some(100))

    case _ => throw new RuntimeException("Illegal arguments")

  }

  @tailrec
  def parseArgs(remain: List[String], results: Args = mutable.Set()): Args =
    remain match {
      case pref :: value :: rem =>
        if (pref.startsWith("-")) results.add(Arg(pref, value))
        parseArgs(rem, results)
      case Nil => results
    }

  def parseSGF(dir: String, outs: Seq[OutputStorage], limit: Option[Int] = None) = {
    try {
      listFilesIn(dir, limit, Some(".sgf")).par foreach { f =>
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
  def processParseResult(result: Collection): Option[(Seq[State], Seq[Move])] = {
    result match {
      // result.successful is guaranteed by caller of this method
      case Collection(List(GameTree(Sequence(nodes: List[Node]), _))) =>
        if (nodes.isEmpty) return None

        var rank: Option[String] = None
        // ============================================
        // Header of sgf
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
            // moves
            // ============================================
            val dummyMv = Move('?', '?', '?', isValid=false)
            val (states, moves) =
              (ArrayBuffer(State(rank=rnk, prevMove=dummyMv)),ArrayBuffer(dummyMv))

            nodes.tail.foreach {
              _.props match {
                // the move
                case List(Property(PropIdent(col: String), List(PropValue(Point(a: Char, b: Char))))) =>
                  val mv = Move(if (col.head == 'W') White else Black, a-'a', b-'a', isValid=true)
                  if (mv.x <= 18 && mv.y <= 18) {
                    states append states.last.nextStateBy(mv)
                    moves append mv
                  }
                // not a move
                case _ =>
              }
            }

            Some((states.init, moves.tail))
          case _ => None
        }
    }
  }

}
