import Implicits._
import SGF._
import Color._
import java.nio.charset.{MalformedInputException => fmtErr}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// sbt "run -m db -c wb -s 4 -d path/to/sgfDir"
object Main {

  def main(args: Array[String]) = {
    val res          = Utils.parseArgs(args.toList)
    val dir          = res find (_._1.matches("-d||--dir"))
    val color        = res find (_._1.matches("-c||--color"))
    val mode         = res find (_._1.matches("-m||--mode"))
    val step         = res find (_._1.matches("-s||--step")) // prediction step num
    val prevStep     = res find (_._1.matches("-p||--prev"))

    (dir, color, mode, step, prevStep) match {
      case (Some(d), Some(c), Some(m), Some(s), Some(p)) if m._2 == "db" => // use sqlite3
        parseSGF(d._2, colorsFrom(c._2).map(new DB(_)), s._2.head-'0', p._2.head-'0')

      case (Some(d), Some(c), Some(m), Some(s), Some(p)) if m._2 == "f" => // use text files
        parseSGF(d._2, colorsFrom(c._2).map(new Files(_)), s._2.head-'0', p._2.head-'0')

      case (Some(d), _, _, Some(s), Some(p)) => // test
        parseSGF(d._2, Seq(), s._2.head-'0', p._2.head-'0', limit=Some(5))

      case (_, Some(c), Some(m), Some(s), Some(o)) if m._2 == "gtp" =>
        GTP_CmdHandler.listenAndServe()

      case _ => throw new IllegalArgumentException("No valid arguments were given.")
    }
  }

  private def parseSGF(dir: String, outs: Seq[OutputStorage],
                       step: Int, prevStep: Int, limit: Option[Int]=None) = try {
    Config.numPrevMoves = prevStep
    Utils.listFilesIn(dir, limit, Some(".sgf")) foreach { f =>
      // getLines() may throw exception
      val parsed = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
      if (parsed.successful) processParseResult(parsed.get, outs.map(_.color)).foreach { pRes =>
        val res = distributeTargetMoves(pRes, step)
        res foreach { x =>
          //if (outs.isEmpty) TargetDistributionTest(x)
          commitResult(x, outs)
        }
      }
    }
  } catch   { case e: fmtErr => println("ignore strange file")
  } finally { outs foreach (_.close()) }

  private def commitResult(res: Seq[(State, Seq[Move])], outs: Seq[OutputStorage]) =
    res foreach { case (st, ts) =>
      outs.foreach{ o => if (o.color == ts.head.color) o.commit(st, ts) }
    }

  private def distributeTargetMoves(res: (Seq[State],Seq[Move]), step: Int): Option[Seq[(State, Seq[Move])]] = {
    val (states, moves) = res
    val (stLen, mvLen) = (states.size, moves.size)
    assert(stLen == mvLen)
    if (stLen > 10) Some(Range(0, stLen-step).map( i => (states(i), moves.slice(i, i + step)) ))
    else None
  }

  /**
    * Main task. Convert case classes to Strings
    *
    * @param result result in 'one' file
    * @return (current states, targets wrt those)
    */
  def processParseResult(result: Collection, colors: Seq[Char]): Option[(Seq[State], Seq[Move])] = {
    result match {
      case Collection(List(GameTree(Sequence(nodes: List[Node]), _))) =>
        if (nodes.isEmpty) return None

        var rankW: Option[String] = None
        var rankB: Option[String] = None

        // ============================================
        // Header of sgf
        // ============================================
        nodes.head.props foreach { prop: Property =>
          prop match {
            // rank
            case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String)))) if a == "WR" =>
              rankW = Some(r).filter(x => x.isValidRank && x.isStrongRank)
            case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String)))) if a == "BR" =>
              rankB = Some(r).filter(x => x.isValidRank && x.isStrongRank)
            // others
            case _ =>
          }
        }
        // ============================================
        // Header end
        // ============================================

        val dummyMv = Move(White, '?', '?', isValid=false)
        val states = ArrayBuffer(State(rankW=rankW, rankB=rankB, prevMoves=Seq(dummyMv)))
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
                // ↓ Do not change the order! Move appending should be the first.
                moves append mv
                states append states.last.nextStateBy(moves)
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
