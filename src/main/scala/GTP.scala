import java.io._

import Color._
import Implicits._

sealed abstract class Cmd {
  protected final def sendEmptyOkResponse() = println("= \n")
  protected final def sendResponse(str: String) = println(s"= $str \n")
  // Main task of the command.
  // This function has side effects. the return value will be to stdout
  def apply(args: Array[String])
}


// ====================================================================
//
//  Implementations of Gtp Commands subset
//
// ====================================================================


// arguments move
// effects A stone of the requested color is played at the requested
// output none
// fails syntax error, illegal move. In the latter case, fails with the
// comments Consecutive moves of the same color are not considered
// move move - Color and vertex of the move
// vertex. The number of captured stones is updated if
// needed and the move is added to the move history.
// error message “illegal move”.
// illegal from the protocol point of view.
object Play extends Cmd {

  // ex) args = [black, B13]
  def apply(args: Array[String]) = {
    GameState.whoToPlayNext = Config.ownColor
    new File("action.txt").append("Play of opponent")
    new File("oppo.txt").append(args.mkString + "\n")
    val color = if (args.head == "white") White else Black

    if (args(1) startsWith "pass") {
      GameState updateBy Move(color, -1, -1, isValid=true, pass=true)
    } else {
      GameState updateBy createMove(color, args(1))
    }

    sendEmptyOkResponse()
  }

  // ex) color ... white
  // ex) pos ... L19
  def createMove(color: Char, pos: String): Move = {
    val x = convertX(pos.head.toLower)
    val y = convertY(pos.tail)
    Move(color, x, y, isValid = true)
  }

  // gtp => sgf Int
  // ex) 'a' => 0
  def convertX(x: Char): Int = (if (x < 'i') x else x - 1) - 'a'

  // gtp => sgf Int
  // ex) "19": String => 0: Int
  def convertY(y: String): Int = {
    // simply str2int
    val num = {
      if (y.length == 1) y.head.toNum // ex) 5
      else 10 + y(1).toNum // ex) 13
    }
    19 - num
  }

}

// arguments: my own color
// effects: A stone of the requested color is played where the engine
// output: vertex
// comments: Notice that “pass” is a valid vertex and should be returned
// color color - Color for which to generate a move.
// chooses. The number of captured stones is updated if
// needed and the move is added to the move history.
// vertex|string vertex - Vertex where the move was played or the string “resign”.
// If the engine wants to pass. Use “resign” if you want to give up the game.
// The controller is allowed to use this command for either color,
// regardless who played the last move.
object GenMove extends Cmd {

  val process = Runtime.getRuntime.exec("python scripts/predict_move.py")
//  val process = new ProcessBuilder("./scripts/predict_move.py").start()
  val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
  val writer = new BufferedWriter(new OutputStreamWriter(process.getOutputStream))

  def apply(args: Array[String]) = {

    if (GameState.whoToPlayNext != Config.ownColor) { // opponent passed and then this method called
      GameState updateBy Move(Config.opponentColor, -1, -1, isValid=true, pass=true)
    } else { // usual
      GameState.whoToPlayNext = Config.opponentColor
    }

    val color = args.head
    val state = GameState.lastState

    if (state.invalidChannel.forall(_ == '1')) {
      sendResponse("pass")
    } else {
      val ch = state.toChannels(Config.ownColor).get
      val invalid = state.invalidChannel.mkString

    else {
      val cmd = s"python scripts/predict_move_multi.py -b ${state.toChannels.get} -i ${state.invalidChannel.mkString}"
      val pos = Utils.execCmd(cmd).init.toInt
      val (x, y) = pos.toCoordinate
      val move = Move(if (color == "white") White else Black, x, y, isValid=true)

      GameState updateBy move
      // return to stderr
      val (xGtp, yGtp) = (if (x.toAlpha < 'i') x.toAlpha else (x+1).toAlpha, Config.dia - y)

      val res = s"${Character.toUpperCase(xGtp)}$yGtp"
      sendResponse(res)
    }
  }
}

object ProtocolVersion extends Cmd {
  def apply(args: Array[String]) = sendResponse("2")
}

object Name extends Cmd {
  def apply(args: Array[String]) = sendResponse("deepgo")
}

object Version extends Cmd {
  def apply(args: Array[String]) = sendResponse("1")
}

object ClearBoard extends Cmd {
  def apply(args: Array[String]) = {
    GameState.reset()
    sendEmptyOkResponse()
  }
}

// Did'nt support showboard but this is required from protocol.
// I leave it to the opponent engine...
object ShowBoard extends Cmd {
  def apply(args: Array[String]) =
    sendResponse(GameState.states.last.board.stateAsString(Config.dia, Config.dia))
}

// argument: mew komi (float)
// effect change komi
// output none
object Komi extends Cmd {
  def apply(args: Array[String]) = {
    Config.Komi = args.head.toFloat
    sendEmptyOkResponse()
  }
}

// arguments none
// effects The session is terminated and the connection is closed.
// output none
object Quit extends Cmd {
  def apply(args: Array[String]) = sendEmptyOkResponse()
}

// arguments none
// effects none
// output commands
// string& commands - List of commands, one per row
object ListCommands extends Cmd {
  def apply(args: Array[String]) = sendResponse(CmdList().mkString(" "))
}

// arguments command name
// effects none
// output known
// string command name - Name of a command
// boolean known - “true” if the command is known by the
// engine, “false” otherwise
object KnownCommand extends Cmd {
  def apply(args: Array[String]) =
    sendResponse(s"${CmdList().contains(args.head)}")
}

object CmdList {
  def apply() = Array(
    "play", "clear_board", "name", "protocol_version", "boardsize",
    "list_commands", "komi", "genmove", "version", "quit", "known_command"
  )
}

// arguments size
// effects The board size is changed. The board configuration, number of captured stones, and move history become arbitrary.
// output none
// fails Syntax error. If the engine cannot handle the new size,
// comments In GTP version 1 this command also did the work of
// int size - New size of the board.
// fails with the error message ”unacceptable size”.
// clear board. This may or may not be true for implementations of GTP version 2. Thus the controller must
// call clear board explicitly. Even if the new board size is
// the same as the old one, the board configuration becomes arbitrary.
object BoardSize extends Cmd {
  def apply(args: Array[String]) = {
    Config.dia = args.head.toInt
    sendEmptyOkResponse()
  }
}



object GameState {

  import scala.collection.mutable.ArrayBuffer

  val states = ArrayBuffer[State]()
  val moves = ArrayBuffer[Move]()

  def updateBy(move: Move) = {
    new File("log.txt").write("update by " + move.toString)
    moves append move
    states append lastState.nextStateBy(moves.toArray)
  }

  var whoToPlayNext = Black

  def lastState = states.last
  def lastMove = moves.last

  // initiative is black
  def reset() = {
    statesclear()
    moves.clear()

    moves.append(Move(Config.opponentColor,'?','?', isValid=false))

    states.append(State(
      board = Rules.genInitialBoard(None), // no handicap
      rankW=Some(Config.ownRank),
      rankB=Some(Config.opponentRank),
      prevMoves=moves.toArray))
  }
}


object GTP_CmdHandler {

  import scala.io.StdIn.readLine


  //"python scripts/predict_move.py".run(pio)
  def listenAndServe(): Unit = {

    loop()

    // recursive func
    def loop() {


      var line = readLine()
      // read GTP command
      while (line == null) line = readLine()

      val stdIn = line.split(' ')
      val (cmd, args) = (stdIn.head, stdIn.tail)

      if      (cmd == "genmove")          GenMove(args)
      else if (cmd == "version")          Version(args)
      else if (cmd == "play")             Play(args)
      else if (cmd == "name")             Name(args)
      else if (cmd == "clear_board")      ClearBoard(args)
      else if (cmd == "list_commands")    ListCommands(args)
      else if (cmd == "protocol_version") ProtocolVersion(args)
      else if (cmd == "known_command")    KnownCommand(args)
      else if (cmd == "boardsize")        BoardSize(args)
      else if (cmd == "showboard")        ShowBoard(args)
      else if (cmd == "komi")         	  Komi(args)
      else if (cmd == "quit")            {Quit(args); sys.exit(1)}
      else throw new RuntimeException("Error!! Unsupported Command: " + cmd + "\n\n")
      // ok response from script
      if (readLine().isEmpty) loop()
    }
  }

}
