import java.io.{FileWriter, File}

object Implicits {

  import Color._

  implicit class RichString(val x: String) extends AnyVal {

    // 9ch (tested)
    def toRankChannel: String =
      (x.charAt(0).getNumericValue, x.charAt(1)) match {
        case (_, 'k') => Utils.zeros(Config.all*9).mkString
        case (_, 'p') => Utils.ones(Config.all*9).mkString
        case (r, 'd') if r == 9 => Utils.ones(Config.all*9).mkString
        case (r, 'd') =>
          val (start, end) = (Config.all*(r-1), Config.all*(r-1) + Config.all)
          val dst = Utils.zeros(Config.all * 9); (start until end).foreach{ dst(_) = '1' }
          dst.mkString
        case _ => throw new RuntimeException("should not happen")
      }

    def isStrong: Boolean = {
      if (x.length != 2) false
      else
        (x.charAt(0).getNumericValue, x.charAt(1)) match {
          case (_, 'k') => false
          case (_, 'p') => true
          case (r, 'd') => true
          case _ => throw new RuntimeException("should not happen")
        }
    }

  }

  implicit class RichFile(val f: File) {
    def appendChar(c: Char): Unit = {
      if (!f.exists()) f.createNewFile()
      val file = new FileWriter(f, true)
      file.write(c)
      file.close()
    }
    def write(str: String): Unit = {
      if (!f.exists()) f.createNewFile()
      val file = new FileWriter(f, false)
      file.write(str + "\n")
      file.close()
    }
    def append(str: String): Unit = {
      if (!f.exists()) f.createNewFile()
      val file = new FileWriter(f, true)
      file.write(str + "\n")
      file.close()
    }
  }

  implicit class RichFloatArray(val x: Array[Float]) extends AnyVal {
    def printSelf(row: Int, col: Int) = {
      assert(x.length == row * col)
      // print
      for (i <- 0 until row) {
        for (j <- 0 until col) {
          print(x(col * i + j) + " ")
        }
        println()
      }
      println("\n")
    }
  }

  implicit class RichIntArray(val x: Array[Int]) extends AnyVal {

    // 1ch (tested with rand. simply int to char conversion)
    def toHistoryChannel: String = {
      val dst = x.map{ a => if (a > 9) '9' else ('0'+a).toChar }
      // assert(dst.length/Constants.all == 1)
      dst.mkString
    }

    def nextHistory(prevBoard: Array[Char], curBoard: Array[Char]) =
      Rules.nextLifespans(x, prevBoard, curBoard)

    def printSelf(row: Int, col: Int) = {
      assert(x.length == row * col)
      // print
      for (i <- 0 until row) {
        for (j <- 0 until col) {
        }
      }
      println("\n")
    }

    def printState(row: Int, col: Int) = {
      assert(x.length == row * col)
      // print
      for (i <- 0 until row) {
        val sb = new StringBuilder
        for (j <- 0 until col) {
          sb.append(('0' + x(col * i + j)).toChar)
        }
        println(sb.toString + "\n")
      }
      println("\n")
    }

    def clip(row: Int, col: Int, borderWidth: Int): Array[Int] =
      Utils.clip(x, row, col, borderWidth)

  }

  implicit class RichCharArray(val in: Array[Char]) extends AnyVal {
    
    def printSelf() = {
      for (x <- in) print(x)
      println()
    }

    def printSelf(row: Int, col: Int) = {
      assert(in.length == row * col)
      var n = 0
      for (i <- 0 until row) {
        val a = in.slice(n, n + col)
        for (j <- 0 until col) print(a(j))
        println()
        n += col
      }
      println()
    }

    def findKoBy(move: Move, newBoard: Array[Char]): Int = {
      val dst = Rules.findKo(move, in, newBoard)
      dst
    }

    // 3ch
    // tested
    def toBoardChannel: String = {
      val (empty, white, black) = (
        Utils.zeros(Config.all),
        Utils.zeros(Config.all),
        Utils.zeros(Config.all)
        )
      var i = 0
      while (i < Config.all) {
        val color = in(i)
        if      (color == Empty) empty(i) = '1'
        else if (color == White) white(i) = '1'
        else if (color == Black) black(i) = '1'
        i += 1
      }
      val dst = empty ++ (white ++ black)
      dst.mkString
    }

    // tested
    def toGroupSizeChannel: String = {
      val gSizes = Rules.groupSizes(in)
      val dst = Array.fill(Config.all * 2)('0')
      in.indices foreach { i =>
        val tmp = gSizes(i)
        val value = if (tmp > 9) 9 else tmp
        if      (in(i) == White) dst(i)     = ('0' + value).toChar
        else if (in(i) == Black) dst(i+361) = ('0' + value).toChar
      }
      dst.mkString
    }

    // 6ch
    def toLibertyChannel = {
      val liberties = Rules.liberties(in)
      val dst = Array.fill(Config.all * 6)('0')

      var i = 0
      while (i < Config.all) {
        val lib = liberties(i)
        val col = in(i)
        if (lib == 1) {
          if      (col == White) dst(i) = '1'
          else if (col == Black) dst(Config.all * 3 + i) = '1'
        }
        else if (lib == 2) {
          if      (col == White) dst(Config.all + i) = '1'
          else if (col == Black) dst(Config.all * 4 + i) = '1'
        }
        else if (lib >= 3) {
          if      (col == White) dst(Config.all * 2 + i) = '1'
          else if (col == Black) dst(Config.all * 5 + i) = '1'
        }
        i += 1
      }
      dst.mkString
    }

    // 1ch
    // tested
    def toBorderChannel = {
      val borderState = Utils.zeros(Config.all)
      Utils.borderPositions(Config.dia) foreach { i =>
        val a = in(i)
        if (a != Empty) borderState(i) = '1'
      }
      borderState.mkString
    }

    // delete stones by move and return new board
    def createNextBoardBy(move: Move): Array[Char] = {
      Rules.boardAfterCaptured(move, in)
    }

    def pad(row: Int, col: Int, padSize: Int, padElem: Char) =
      Utils.pad(in, row, col, padSize, padElem)

    def printState(row: Int, col: Int, move: Option[Move], ko: Option[Int]) = {
      assert(in.length == row * col)

      val x = in.clone()

      // ko
      ko.foreach{ k => if (k != -1) x(k) = 'K' }

      // move
      move.foreach{ move => x(move.pos) = if (move.color == White) 'W' else 'B' }

      // actual print
      print("  ")
      Array.range(0, Config.dia).foreach{i => print(('a' + i).toChar + " ") }
      println()
      for (i <- 0 until row) {
        print(('a' + i).toChar + " ")
        for (j <- 0 until col) print(x(col * i + j) + " ")
        println()
      }
      println("\n")
    }

    def clip(row: Int, col: Int, borderWidth: Int): Array[Char] =
      Utils.clip(in, row, col, borderWidth)

  }

  implicit class RichChar(val a: Char) extends AnyVal {
    def opponent: Char = {
      if (a == White)  Black
      else if (a == Black)  White
      else throw new RuntimeException(s"move color = $a should not occur!")
    }
    def isOpponentOf(x: Char): Boolean = {
      if (x == White) a == Black || a == Outside
      else if (x == Black) a == White || a == Outside
      else throw new RuntimeException(s"move color = $x should not occur!")
    }
    def isStone = a == White || a == Black
    // ex) '9' => 9
    // don't do  'a' => foo
    def toNum: Int = a - '0'
  }

  implicit class RichInt(val value: Int) extends AnyVal {

    def isSuicideMovePos(color: Char, in: Array[Char]): Boolean = {
      val (x, y) = value.toCoordinate
      Rules.isSuicideMove(Move(color, x, y, isValid = true), in)
    }

    // tested
    // (x, y)
    // top-left is 0. pos is based on this
    def toCoordinate: (Int, Int) = {
      val y = value / Config.dia
      val x = value - (Config.dia * y)
      (x, y)
    }

    // tested
    // 0 => a
    // 1 => b
    def toAlpha: Char = ('a' + value).toChar

    // 1ch
    def toKoChannel = {
      val dst = Utils.zeros(Config.all)
      if (value != -1) dst(value) = 1
      dst.mkString
    }

    /**
     * If the side length of the square changed, for example 21 -> 19,
     * position in the square will change. This method return that changed position.
     * I could'nt come up with the reasonable method name.
     *
     * @param from current diameter
     * @param to diameter after clipped
     * @return position after that
     */
    def rectify(from: Int, to: Int): Int = {
      assert(from > to)
      if (value < 0) return -1
      val borderW = (from - to ) / 2
      var paddedY = 0
      var t = from
      while (t - 1 < value) {
        paddedY += 1
        t += from
      }
      val paddedX = value - from * paddedY
      val clippedY = paddedY - borderW
      val clippedX = paddedX - borderW
      val dst = to * clippedY + clippedX
      dst
    }
  }

}
