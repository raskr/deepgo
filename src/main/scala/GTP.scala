import Colors._
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine
import Implicits._
import Utils._

object GTP_CmdHandler {

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
      else if (cmd == "komi")         	  Komi(args)
      else if (cmd == "quit")             return
      else {
        throw new RuntimeException("Error!! Unsupported Command: " + cmd + "\n\n")
      }
      // ok response from script
      if (readLine().isEmpty) loop()
    }
  }

}

object GameState {
  val states = ArrayBuffer[State]()
  def updateBy(move: Move) = states.append(states.last.createNextBy(move))
  def currentState = states.last
  def reset() = { states.clear(); states.append(State(rank="1d")) }
}

object CmdList {
  def apply() = Array(
    "play", "clear_board", "name", "protocol_version", "boardsize",
    "list_commands", "komi", "genmove", "version", "quit", "known_command"
  )
}

sealed abstract class Cmd {
  protected final def sendEmptyOkResponse() = println("= \n")
  protected final def sendResponse(str: String) = println(s"= $str \n")
  // This function has many side effect. the return value will be stdout
  def apply(args: Array[String])
}


// ====================================================================
//
//  Implementations of subset of Gtp Commands
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
    val move = createMove(args.head, args(1))
    GameState updateBy move
    sendEmptyOkResponse()
  }

  def createMove(color: String, pos: String): Move = {
    val x = convertX(pos.head.toLower)
    val y = convertY(pos.tail)
    Move(if (color == "white") White else Black, x, y)
  }

  def convertX(x: Char): Char = (if (x < 'i') x else x - 1).toChar

  // ex) "19": String => a: Char
  def convertY(y: String): Char = {
    // simply str2int
    val num = {
      if (y.length == 1) y.head.toNum // ex) 5
      else 10 + y(1).toNum // ex) 13
    }
    // 1. change the origin of y-axis
    // 2. make zero origin
    // 3. to alphabet
    (19 - num - 1).toAlpha
  }

}

object ClearBoard extends Cmd {
  def apply(args: Array[String]) = {
    GameState.reset()
    sendEmptyOkResponse()
  }
}

// arguments none
// effects none
// output name
// fails never
// string* name - Name of the engine
object Name extends Cmd {
  def apply(args: Array[String]) = sendResponse("deepgo")
}

// arguments none
// effects none
// output version number
// int version number - Version of the GTP Protocol
object ProtocolVersion extends Cmd {
  def apply(args: Array[String]) = sendResponse("2")
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
  var dia = -1
  def apply(args: Array[String]) = {
    dia = args.head.toInt
    sendEmptyOkResponse()
  }
}

// argument: mew komi (float)
// effect change komi
// output none
object Komi extends Cmd {
  var komi = 0f
  def apply(args: Array[String]) = {
    komi = args.head.toFloat
    sendEmptyOkResponse()
  }
}
// arguments none
// effects none
// output commands
// string& commands - List of commands, one per row
object ListCommands extends Cmd {
  def apply(args: Array[String]) =
    sendResponse(CmdList().mkString(" "))
}

// arguments color
// effects A stone of the requested color is played where the engine
// output vertex
// fails never
// comments Notice that “pass” is a valid vertex and should be returned
// color color - Color for which to generate a move.
// chooses. The number of captured stones is updated if
// needed and the move is added to the move history.
// vertex|string vertex - Vertex where the move was
// played or the string “resign”.
// if the engine wants to pass. Use “resign” if you want to
// give up the game. The controller is allowed to use this
// command for either color, regardless who played the last move.
object GenMove extends Cmd {
  def apply(args: Array[String]) = {
    // update state
    val color = args.head
    val state = GameState.states.last

    val cmd = s"python scripts/predict_move.py -b ${state.toChannels} -i ${state.invalidChannel} -c $color"
    // TODO: reduce the command execution (For now execute every time genmove called)
    val pos = execCmd(cmd).init.toInt

    val (x, y) = pos.toCoordinate
    val (xAlpha, yAlpha) = (x.toAlpha, y.toAlpha)
    val move = Move(if (color == "white") White else Black, xAlpha, yAlpha)
    GameState updateBy move

    // return to stderr
    val (xGtp, yGtp) = (if (xAlpha < 'i') xAlpha else (xAlpha+1).toChar, Constants.dia - y)
    sendResponse(s"${Character.toUpperCase(xGtp)}$yGtp")
  }
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

// arguments none
// effects none
// output version
// string* version - Version of the engine
object Version extends Cmd {
  def apply(args: Array[String]) = sendResponse("1")
}

// arguments none
// effects The session is terminated and the connection is closed.
// output none
object Quit extends Cmd {
  def apply(args: Array[String]) =
    throw new NotImplementedError("Quit process without calling this method.")
}