import java.io.{File, PrintStream}
import Color._
import java.nio.charset.{MalformedInputException => fmtErr}
import SGF._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import Implicits._

object TopLevelTask {

  def parseSGF(dir: String, outs: Seq[OutputStorage],
                       step: Int, limit: Option[Int]=None) = try {
    Utils.listFiles(dir, limit, Some(".sgf")).par foreach { f =>
      // getLines() may throw mal format exception
      val parsed = SGF.parseAll(SGF.pAll, Source.fromFile(f).getLines.mkString)
      if (parsed.successful) {
        processParseResult(parsed.get) foreach { pRes =>
          assignTargetMoves(pRes, step) foreach { results => // res is `one` state and targets
            for {
              (st, ts) <- results
              out <- outs
            } yield if (ts.head.color == out.color) out.commit(st, ts)
          }
        }
      }

    }
  } catch   {
    case e: fmtErr => println("ignore strange file")
    case e: Exception => e.printStackTrace(new PrintStream(new File("log")))
  } finally { outs foreach (_.close()) }


  /**
    * Assign target moves to each state in accordance with prediction step num.
    *
    * @param res Sequence of tuple (state, move)
    * @param step prediction step
    */
  private def assignTargetMoves(res: (Seq[State],Seq[Move]), step: Int): Option[Seq[(State, Seq[Move])]] = {
    val (states, moves) = res
    val stLen = states.size
    if (stLen < 10) { // only use over ``10-move game``
      None
    } else {
      val a = Some(Range(0, stLen-step).map(i => (states(i), moves.slice(i, i+step))))
      if (Config.DEBUG) TargetAssigningTest(a.get)
      a
    }
  }

  /**
    * Main task. Convert case classes to Strings
    *
    * @param result result in 'one' file
    * @return (current states, targets wrt those)
    */
  def processParseResult(result: Collection): Option[(Seq[State], Seq[Move])] = result match {
    case Collection(List(GameTree(Sequence(nodes: List[Node]), _))) =>
      // This can occur? I don't know.
      if (nodes.isEmpty) return None

      var rankW: Option[String] = None
      var rankB: Option[String] = None

      // handicap can change these 2 variables
      var initialBoard = Utils.empties(Config.all)
      var initialPlayer = Black

      // ============================================
      // Header of sgf
      // ============================================
      nodes.head.props foreach { prop: Property =>
        prop match {
          // rank White
          case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String)))) if a == "WR" =>
            rankW = Some(r).filter(x => x.isValidRank && x.isStrongRank)

          // rank Black
          case Property(PropIdent(a: String), List(PropValue(SimpleText(r: String)))) if a == "BR" =>
            rankB = Some(r).filter(x => x.isValidRank && x.isStrongRank)

          // handicap for Black
          case Property(PropIdent(id: String), mvs: List[PropValue]) if id == "AB" =>
            initialBoard = mvs.foldLeft(Utils.empties(361)){ (prevBoard, mv) => mv match {
              case PropValue(Point(a: Char, b: Char)) =>
                prevBoard.createNextBoardBy(Move(Black, a-'a', b-'a', isValid=true))
            }}
            initialPlayer = White

          // handicap for White.
          case Property(PropIdent(id: String), mvs: List[PropValue]) if id == "AW" =>
            initialBoard = mvs.foldLeft(Utils.empties(361)){ (prevBoard, mv) => mv match {
              case PropValue(Point(a: Char, b: Char)) =>
                prevBoard.createNextBoardBy(Move(White, a-'a', b-'a', isValid=true))
            }}
            initialPlayer = Black

          // others
          case _ =>
        }
      }
      // ============================================
      // Header end
      // ============================================

      val states = ArrayBuffer(State(board=initialBoard, ansColor=initialPlayer, rankW=rankW, rankB=rankB))
      val moves = ArrayBuffer[Move]()
      var pass = 0

      // ============================================
      // moves
      // ============================================
      nodes.tail.foreach {
        _.props match {
          // a move
          case List(Property(PropIdent(col: String), List(PropValue(Point(a: Char, b: Char))))) =>
            val move = Move(if (col.head.toLower == 'w') White else Black, a-'a', b-'a', isValid=true)
            moves append move
            states append states.last.nextStateBy(move)

          // pass
          case List(Property(PropIdent(col: String), List())) =>
            pass += 1

          // not a move
          case _ =>
        }
      }
      // ============================================
      // moves end
      // ============================================

      // If player passed three times, we don't treat this game.
      // Two times pass is ok. It may means usual game ending
      if (pass >= 3)
        None
      else
        Some((states.init, moves))
  }


}
