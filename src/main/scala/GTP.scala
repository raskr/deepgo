import Color._
import Implicits._

sealed abstract class Cmd {
  protected final def sendEmptyOkResponse() = println("= \n")
  protected final def sendResponse(str: String) = println(s"= $str \n")
  // Main task of the command.
  // This function has many side effect. the return value will be stdout
  def apply(args: Array[String])
}


// ====================================================================
//
//  Implementations of Gtp Commands subset
//
// ====================================================================

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
  def apply(args: Array[String]) = sendResponse("stub!!!!")
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
    val move = createMove(args.head, args(1))
    GameState updateBy move
    sendEmptyOkResponse()
  }

  // ex) color ... white
  // ex) pos ... L19
  def createMove(color: String, pos: String): Move = {
    val x = convertX(pos.head.toLower)
    val y = convertY(pos.tail)
    Move(if (color == "white") White else Black, x, y, isValid = true)
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
  def apply(args: Array[String]) = {
    // update state
    val color = args.head
    val state = GameState.currentState

    val cmd = s"python scripts/predict_move.py -b ${state.toChannels} -i ${state.invalidChannel} -c $color"
    // TODO: reduce the command execution (For now execute python script every time genmove called)
    // `init` get rid of the "\n"
    val pos = Utils.execCmd(cmd).init.toInt

    val (x, y) = pos.toCoordinate
    val (xAlpha, yAlpha) = (x.toAlpha, y.toAlpha)

    val move = Move(if (color == "white") White else Black, xAlpha, yAlpha, isValid=true)
    GameState updateBy move
    // return to stderr
    val (xGtp, yGtp) = (if (xAlpha < 'i') xAlpha else (xAlpha+1).toChar, Config.dia - y)
    sendResponse(s"${Character.toUpperCase(xGtp)}$yGtp")
  }
}

class GTP_CmdHandler(opponentRank: String, myColor: Char) {

  import scala.io.StdIn.readLine

  if (TextUtils.isEmpty(opponentRank)) throw new RuntimeException("opponentRank is required")
  GameState.opponentRank = opponentRank
  GameState.myColor = myColor

  def listenAndServe(): Unit = {

    loop()

    // recursive func
    def loop() {
      var line = readLine()
      while (line == null) line = readLine()
      val stdin = line.split(' ')

      val (cmd, args) = (stdin.head, stdin.tail)

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
      else {
        throw new RuntimeException("Error!! Unsupported Command: " + cmd + "\n\n")
      }
      // ok response from script
      if (readLine().isEmpty) loop()
    }
  }

}

object GameState {
  import scala.collection.mutable.ArrayBuffer
  var opponentRank = ""
  var myColor = ' '
  val states = ArrayBuffer[State]()
  def updateBy(move: Move) = states.append(states.last.nextStateBy(move))
  def currentState = states.last
  def reset() = {
    states.clear()
    if (myColor == White) {
      states.append(State(rankW=Some(opponentRank), rankB=None, prevMove=Move('?','?','?', isValid=false)))
    } else {
      states.append(State(rankB=Some(opponentRank), rankW=None, prevMove=Move('?','?','?', isValid=false)))
    }
  }
}
