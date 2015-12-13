import java.io.{FileWriter, File}

object Implicits {

  import Colors._
  import Utils._
  import scala.collection.mutable

  implicit class RichString(val x: String) extends AnyVal {

    // 9ch
    def toRankChannel: String =
      (x.charAt(0).getNumericValue, x.charAt(1)) match {
        case (_, 'p') => ones(Constants.all*9).mkString
        case (r, 'd') if r == 9 => ones(Constants.all*9).mkString
        case (r, 'd') =>
          val (start, end) = (Constants.all*(r-1), Constants.all*(r-1) + Constants.all)
          val dst = zeros(Constants.all * 9); (start until end).foreach{ dst(_) = '1' }
          assert(dst.length/Constants.all == 9)
          dst.mkString
        case _ => throw new RuntimeException("should not happen")
      }

    // rank.isStrong
    def isStrong: Boolean = {
      if (x.length != 2) false
      else
        (x.charAt(0).getNumericValue, x.charAt(1)) match {
          case (_, 'k') => false
          case (_, 'p') => true
          case (r, 'd') => true
          case _ => false
        }
    }

  }

  implicit class RichFile(val f: File) {
    def write(str: String): Unit = {
      if (!f.exists()) f.createNewFile()
      val file = new FileWriter(f, true)
      file.write(str)
      file.close()
    }
  }

  implicit class RichIntArray(val x: Array[Int]) extends AnyVal {

    // 1ch (tested with rand. simply int to char conversion)
    def toLifespanChannel: String = {
      val dst = x.map{ a => if (a > 9) '9' else ('0'+a).toChar }
      assert(dst.length/Constants.all == 1)
      dst.mkString
    }

    // 1ch
    def nextLifespans(prevBoard: Array[Char], curBoard: Array[Char]) = {
      val dst = (0 until Constants.all).toArray map { i =>
        val prev = prevBoard(i)
        val cur = curBoard(i)
        if (prev == cur && prev != Empty) x(i)+1 else 0
      }
       assert(dst.length/Constants.all == 1)
      dst
    }

    def printState(row: Int, col: Int) = {
      assert(x.length == row * col)
      // print
      for (i <- 0 until row) {
        for (j <- 0 until col) print(x(col * i + j))
        println()
      }
      println("\n")
    }

    def clip(row: Int, col: Int, borderWidth: Int) = {
      assert(x.length == row * col)
      val dstRow = row - borderWidth * 2
      val dstCol = col - borderWidth * 2
      val dst = Array.fill(dstRow * dstCol)(0)
      var i, j = 0
      while (i < dstRow) {
        j = 0
        while (j < dstCol) {
          dst(dstCol*i+ j) = x((i+borderWidth) * col + (j + borderWidth))
          j += 1
        }
        i += 1
      }
      dst
    }

  }

  implicit class RichCharArray(val in: Array[Char]) extends AnyVal {
    
    def printSelf() = {
      for (x <- in) print(x)
      println()
    }

    def printSelf(row: Int, col: Int) = {
      assert(in.length == row * col)
      var n = 0
      for (i <- 0 until row){
        val a = in.slice(n, n + col)
        for (j <- 0 until col) print(a(j))
        println()
        n += col
      }
      println()
    }

    def findKoBy(move: Move): Int = {
      val padded = in.pad(Constants.dia, Constants.dia, 1, Outside)
      val movePos = (move.y+1)*21 + (move.x+1)
      val around = Seq(movePos + 1, movePos - 1, movePos + 21, movePos - 21)
      val surrounded = around forall { padded(_) isOpponentOf move.color }
      val empty4 = around.filter { padded(_) == Empty }
      val ko21 = if (surrounded && empty4.size == 1) empty4.head else -1
      ko21.rectify(1, from = Constants.dia+2, to = Constants.dia)
    }

    // 3ch
    def toBoardChannel: String = {
      val (empty, white, black) = (zeros(Constants.all), zeros(Constants.all), zeros(Constants.all))
      (0 until Constants.all) foreach { i =>
        val color = in(i)
        if (color == Empty) empty(i) = '1'
        if (color == White) white(i) = '1'
        if (color == Black) black(i) = '1'
      }
      val dst = empty ++ (white ++ black)
      assert(dst.length/Constants.all == 3)
      dst.mkString
    }

    // 6ch
    def toLibertyChannel = {
      val liberties = in.liberties
      val dst = Array.fill(Constants.all * 6)('0')

      (0 until Constants.all) foreach { i =>
        val lib = liberties(i)
        val col = in(i)
        if (lib == 1) {
          if (col == 'W') dst(i) = '1'
          if (col == 'B') dst(Constants.all * 3 + i) = '1'
        }
        if (lib == 2) {
          if (col == 'W') dst(Constants.all + i) = '1'
          if (col == 'B') dst(Constants.all * 4 + i) = '1'
        }
        if (lib >= 3) {
          if (col == 'W') dst(Constants.all * 2 + i) = '1'
          if (col == 'B') dst(Constants.all * 5 + i) = '1'
        }
      }
      assert(dst.length/Constants.all == 6)
      dst.mkString
    }

    // 1ch
    def toBorderChannel = {
      val borderState = zeros(Constants.all)
      borderPositions(Constants.dia) foreach { i =>
        val a = in(i)
        if (a != Empty) borderState(i) = '1'
      }
      assert(borderState.length/Constants.all == 1)
      borderState.mkString
    }

    def liberties: Array[Int] = {
      val padded = in.pad(Constants.dia, Constants.dia, 1, Outside)
      val dst = Array.fill(21*21)(0)

      dst.indices foreach { idx =>
        if (padded(idx).isStone) {
          val blo, skip = mutable.Set[Int]()
          val lib = new MutableInt(0)
          loop(idx, padded(idx), lib, skip, blo)
          blo foreach { x => dst(x) = lib.value }
        }
      }

      // recursive func
      def loop(i: Int, checkColor: Char, lib: MutableInt,
               shouldSkip: mutable.Set[Int], block: mutable.Set[Int]): Unit =
      {
        if (shouldSkip(i) || dst(i) > 0) return
        shouldSkip.add(i)
        val col = padded(i)
        if (col == Empty) lib += 1
        else if (col == checkColor) { // same color
          block.add(i)
          Seq(i+1, i-1, i+21, i-21).filterNot(shouldSkip.contains)
            .foreach {loop(_, checkColor, lib, shouldSkip, block)}
        }
      }
      dst.clip(21, 21, 1)
    }

    // delete stones by move and return new board
    def createNextBy(move: Move) = {
      // pad the input board for convenience
      val padded = in.pad(Constants.dia, Constants.dia, 1, Outside)
      val movePos = (move.y+1)*21 + (move.x+1)
      val booleans: Array[Option[Boolean]] = Array.fill(21 * 21)(None)
      val shouldSkip = mutable.Set[Int]()

      // recursive func
      def loop(i: Int): Boolean = {
        val color = padded(i)
        if (booleans(i).isEmpty) {
          booleans(i) = Some(
            if (color == move.color) true
            else if (color == Outside) true
            else if (color == Empty) false
            else {
              val around = Seq(i+1, i-1, i+21, i-21)
                .filterNot{ shouldSkip.contains }
                .map(x => booleans(x).getOrElse { shouldSkip.add(i); loop(x) })

              val shouldKill = around forall (_ == true)
              if (shouldKill) padded(i) = Empty
              shouldKill
            })
        }
        booleans(i).get
      }

      // 1. Reflect the move itself
      padded(movePos) = move.color
      booleans(movePos) = Some(true)

      // 2. Start inspection around the move-point and remove stones if possible
      Seq(movePos+1, movePos-1, movePos+21, movePos-21) foreach loop

      padded.clip(21, 21, 1)
    }

    def pad(row: Int, col: Int, padSize: Int, padElem: Char) = {
      assert(in.length == row * col)
      val allNum = row * col

      def zeros(n: Int) = Array.fill(n)(padElem)
      def chunk = zeros((col + padSize * 2) * padSize)

      // 1 (chunk + 2)
      var dst: Array[Char] = chunk ++ zeros(padSize)

      var i = 0
      while (i <= allNum - col * 2) {
        // slice one row
        val a = in.slice(i, i + col)
        val b = a ++ zeros(padSize * 2)
        dst = dst ++ b
        i += col
      }
      dst = dst ++ (in.slice(i, i + col) ++ (zeros(padSize) ++ chunk.clone()))
      dst
    }

    def printState(row: Int, col: Int, emphasize: Option[Move], ko: Option[Int]) = {
      assert(in.length == row * col)
      val x = in.map(para => if (para == White) '●' else if (para == Black) '○' else if (para == Empty) '◎')
      assert(x.length == row * col)

      // ko
      ko.foreach{ k => if (k != -1) x(k) = '※' }

      // move
      emphasize.foreach{ move =>
        x(move.pos) = if (move.color == White) '★' else '☆'
      }

      // print
      for (i <- 0 until row) {
        for (j <- 0 until col) print(x(col * i + j))
        println()
      }
      println("\n")
    }

    def clip(row: Int, col: Int, borderWidth: Int) = {
      assert(in.length == row * col)
      val dstRow = row - borderWidth * 2
      val dstCol = col - borderWidth * 2
      val dst = Array.fill(dstRow * dstCol)(Empty)
      var i, j = 0
      while (i < dstRow) {
        j = 0
        while (j < dstCol) {
          dst(dstCol*i+ j) = in((i+borderWidth) * col + (j + borderWidth))
          j += 1
        }
        i += 1
      }
      dst
    }

  }

  implicit class RichChar(val a: Char) extends AnyVal {
    def isOpponentOf(x: Char): Boolean = {
      if (x == White) a == Black || a == Outside
      else if (x == Black) a == White || a == Outside
      else throw new RuntimeException
    }
    def isStone = a == White || a == Black
    def toNum = a - '0'.toInt
  }
  
  implicit class RichInt(val value: Int) extends AnyVal {

    // tested
    // (x, y)
    // top-left is 0. pos is based on this
    def toCoordinate: (Int, Int) = {
      val y = value / Constants.dia
      val x = value - (Constants.dia * y)
      (x, y)
    }
    // 0 => a
    // 1 => b
    def toAlpha: Char = ('a' + value).toChar

    // 1ch
    def toKoChannel = {
      val dst = zeros(Constants.all)
      if (value != -1) dst(value) = 1
      assert(dst.length/Constants.all == 1)
      dst.mkString
    }

    /**
     * @param borderW width of clip
     * @param from current diameter
     * @param to diameter after clipped
     * @return
     */
    def rectify(borderW: Int, from: Int, to: Int): Int = {
      assert(from > to)
      if (value < 0) return -1
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
