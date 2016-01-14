import Implicits._
import SGF._
import Utils._
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
    val opponentRank = res find (_._1.matches("-r||--rank")) // use when gtp mode

    (dir, color, mode, step, opponentRank) match {
      case (Some(d), Some(c), Some(m), Some(s), _) if m._2 == "db" => // use sqlite3
        val stp = s._2.charAt(0)-'0'
//        if (stp == 1)
//          parseSGF_single(d._2, colorsFrom(c._2).map(new DB1(_)), s._2.head-'0', limit=None)
//        else
          parseSGF(d._2, colorsFrom(c._2).map(new DB(_)), stp, limit=Some(110000))

      case (Some(d), Some(c), Some(m), Some(s), _) if m._2 == "f" => // use text files
        parseSGF(d._2, colorsFrom(c._2).map(new Files(_)), s._2.head-'0')

      case (Some(d), _, _, Some(s), _) => // test
        parseSGF(d._2, Seq(), 4, limit=Some(1000))

      case (_, Some(c), Some(m), Some(s), Some(o)) if m._2 == "gtp" =>
        GTP_CmdHandler(o._2, c._2.head).listenAndServe()

      case _ => throw new IllegalArgumentException("No valid arguments were given.")
    }
  }
  def parseSGF_single(dir: String, outs: Seq[OutputStorage], step: Int, limit: Option[Int] = None) = {
    var i = 0
    try {
      listFilesIn(dir, limit, Some(".sgf")).par foreach { f =>
        i += 1; if (i % 1000 == 0) println(i)
        try {
          val res = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
          if (res.successful) processParseResult(res.get, outs.map(_.color)) foreach {
            commitResult(_, outs)
          }
        } catch {
          case e: java.nio.charset.MalformedInputException =>
            println("ignore strange file: " + f.getName)
        }
      }
    } finally outs foreach { _.close() }
  }

  def parseSGF(dir: String, outs: Seq[OutputStorage], step: Int, limit: Option[Int]=None) = try {
    listFilesIn(dir, limit, Some(".sgf")).par foreach { f =>
      // getLines() may throw exception
      val parsed = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines().mkString)
      if (parsed.successful) processParseResult(parsed.get, outs.map(_.color)).foreach { pRes =>
        val res = distributeTargetMoves(pRes, step)
        res foreach { x =>
          if (outs.isEmpty) TargetDistributionTest(x)
          commitResult(x, outs)
        }
      }
    }
  } catch   { case e: fmtErr => println("ignore strange file")
  } finally { outs foreach (_.close()) }

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
