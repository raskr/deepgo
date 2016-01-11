import Implicits._
import SGF._
import Utils._
import Color._
import java.nio.charset.{MalformedInputException => fmtErr}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// sbt "run -m db -c wb -d path_to_sgf_dir"
object Main extends App {

  val res = Utils.parseArgs(args.toList)

  val dir          = res find (_._1 == "-d")
  val color        = res find (_._1 == "-c")
  val mode         = res find (_._1 == "-m")
  val opponentRank = res find (_._1 == "-o") // use when gtp mode

  (dir, color, mode, opponentRank) match {
    case (Some(d), Some(c), Some(m), _) if m._2 == "db" =>
      parseSGF(d._2, colorsFrom(c._2).map(new DB(_)), limit=None)

    case (Some(d), Some(c), Some(m), _) if m._2 == "f" =>
      parseSGF(d._2, colorsFrom(c._2).map(new Files(_)))

    case (_, Some(c), Some(m), Some(o)) if m._2 == "gtp" =>
      GTP_CmdHandler(o._2, c._2.head).listenAndServe()

    case (Some(d), _, _, _) =>
      println("test mode (run mode was not given) ... ")
      parseSGF(d._2, Seq(), limit=Some(100))

    case _ => throw new IllegalArgumentException

  }

  def parseSGF(dir: String, outs: Seq[OutputStorage], limit: Option[Int] = None) = {
    try {
      listFilesIn(dir, limit, Some(".sgf")).par foreach { f =>
        val pRes = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
        if (pRes.successful) processParseResult(pRes.get, outs.map(_.color)).foreach { res =>
          val res = distributeTargetMoves(res, step=1)
          commitResult(res, outs)
        }
      }
    }
    catch { case e: fmtErr => println("ignore strange file"}
    finally outs foreach { _.close() }
  }

  private def commitResult(res: (Seq[State], Seq[Move]), outs: Seq[OutputStorage]) = {
    val (states, moves) = res
    zipEach(states, moves){ (st, mv) =>
      outs.foreach {out => if (out.color == mv.color) out.commit(st, mv) }
    }
  }

  private def commitResult(res: Seq[(State, Seq[Move])], outs: Seq[OutputStorage]) = {
    res foreach { case (st, ts) =>
      outs.foreach{ o => if (o.color == ts.head.color) o.commit(st, ts) }
    }
  }

  def distributeTargetMoves(res: (Seq[State],Seq[Move]),
                            step: Int): Seq[(State, Seq[Move])] = {
    val (states, moves) = res
    val stLen = states.size
    val mvLen = moves.size

    assert(step >= 1 && stLen == mvLen && step < stLen)

    Range(0, stLen-step).map( i => (states(i), moves.slice(i, i + step)) )
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
            case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String)))) if a == "WR" =>
              rankW = Some(r).filter(_.isStrongRank)
            // rank (black player)
            case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String)))) if a == "BR" =>
              rankB = Some(r).filter(_.isStrongRank)
            // other
            case _ =>
          }
        }
        // ============================================
        // Header end
        // ============================================

        val dummyMv = Move(White, '?', '?', isValid=false)
        val states = ArrayBuffer(State(rankW=rankW, rankB=rankB, prevMove=dummyMv))
        val moves = ArrayBuffer(dummyMv)

        // ============================================
        // moves
        // ============================================
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
        // ============================================
        // moves end
        // ============================================

        Some((states.init, moves.tail))
    }
  }

}
