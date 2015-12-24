object Rules {

  import scala.collection.mutable
  import Implicits._
  import Colors._

  // dead position is the pos that liberty is 0
  def deadPositions(move: Move, board: Array[Char]): mutable.Set[Int] = {
    val padded = board.pad(Config.dia, Config.dia, 1, Outside)
    val dst = mutable.Set[Int]()
    val movePos = (move.y+1)*21 + (move.x+1)
    val checked = mutable.Set[Int]()

    Seq(movePos+1, movePos-1, movePos+21, movePos-21) foreach { idx =>
      if (padded(idx).isStone) {
        val blo, skip = mutable.Set[Int]()
        val lib = new MutableInt(0)
        loop(idx, padded(idx), lib, skip, blo)
        blo foreach { x =>
          checked.add(x)
          if (lib.value == 0 && padded(x) != Empty)
            dst.add(x)
        }
      }
    }

    // recursive func
    def loop(i: Int, checkColor: Char, lib: MutableInt,
             shouldSkip: mutable.Set[Int], block: mutable.Set[Int]): Unit =
    {
      if (shouldSkip(i) || checked.contains(i)) return
      shouldSkip.add(i)
      val col = padded(i)
      if (col == Empty) lib += 1
      else if (col == checkColor) { // same color
        block.add(i)
        Seq(i+1, i-1, i+21, i-21).filterNot(shouldSkip.contains)
          .foreach {loop(_, checkColor, lib, shouldSkip, block)}
      }
    }

    dst.map(_.rectify(21, 19))
  }

  def liberties(board: Array[Char]): Array[Int] = {
    val padded = board.pad(Config.dia, Config.dia, 1, Outside)
    val paddedDia = Config.dia + 2
    val dst = Array.fill(paddedDia * paddedDia)(0)

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
        Seq(i+1, i-1, i+paddedDia, i-paddedDia).filterNot(shouldSkip.contains)
          .foreach {loop(_, checkColor, lib, shouldSkip, block)}
      }
    }
    dst.clip(paddedDia, paddedDia, 1)
  }

  def boardAfterCaptured(move: Move, board: Array[Char]): Array[Char] = {
    val dst = board.clone()
    var flag = false
    dst(move.pos) = move.color
    deadPositions(move, dst).foreach{ x =>
      dst(x) = Empty
      flag = true
    }
    // if (flag) board.printState(19, 19, Some(move), None)
    dst
  }

  // This is the same as boardAfterCaptured(). Works perfectly but poor performance
  //  def boardAfterCaptured1(move: Move, _board: Array[Char]): Array[Char] = {
  //    val board = _board.clone()
  //    board(move.pos) = move.color
  //    val libs = liberties(board)
  //    libs.zipWithIndex.foreach{ case (lib, i) =>
  //      if (lib == 0 && board(i) != Empty) board(i) = Empty
  //    }
  //    board
  //  }

  // 1ch
  def nextLifespans(curLifespan: Array[Int],
                    prevBoard: Array[Char],
                    curBoard: Array[Char]): Array[Int] =
  {
    val dst = Array.range(0, Config.all) map { i =>
      val prev = prevBoard(i)
      val cur = curBoard(i)
      if (prev == cur && prev != Empty) curLifespan(i)+1 else 0
    }
    dst
  }

  def groupSizes(in: Array[Char]): Array[Int] = {
    val paddedDia = Config.dia + 2
    val padded = in.pad(Config.dia, Config.dia, 1, Outside)
    val dst = Array.fill(paddedDia*paddedDia)(-1)

    dst.indices foreach { idx =>
      if (padded(idx).isStone) {
        val group, skip = mutable.Set[Int]()
        loop(idx, padded(idx), skip, group)
        val groupSize = group.size
        group foreach { x => dst(x) = groupSize }
      }
    }

    // recursive func
    def loop(i: Int, checkColor: Char,
             shouldSkip: mutable.Set[Int], block: mutable.Set[Int]): Unit =
    {
      if (shouldSkip(i) || dst(i) > -1) return
      shouldSkip.add(i)
      val col = padded(i)
      if (col == checkColor) { // same color
        block.add(i)
        Seq(i+1, i-1, i+paddedDia, i-paddedDia).filterNot(shouldSkip.contains)
          .foreach {loop(_, checkColor, shouldSkip, block)}
      }
    }

    dst.clip(paddedDia, paddedDia, 1).map(x => if (x == -1) 0 else x)
  }

  def findKo(move: Move, prevBoard: Array[Char], newBoard: Array[Char]): Int = {
    val paddedPrev = prevBoard.pad(Config.dia, Config.dia, 1, Outside)
    val paddedNew = newBoard.pad(Config.dia, Config.dia, 1, Outside)
    val movePos = (move.y+1)*21 + (move.x+1)
    val around = Seq(movePos+1, movePos-1, movePos+21, movePos-21)
    val surrounded = around forall { paddedPrev(_) isOpponentOf move.color }
    val empty4 = around.filter { paddedNew(_) == Empty }
    val ko21 = if (surrounded && empty4.size == 1) empty4.head else -1
    ko21.rectify(from = 21, to = Config.dia)
  }

  def isSuicideMove(move: Move, board: Array[Char]): Boolean = {
    val padded = board.pad(Config.dia, Config.dia, 1, Outside)
    val movePos = (move.y+1)*21 + (move.x+1)

    if (padded(movePos+1) == Empty) return false
    if (padded(movePos-1) == Empty) return false
    if (padded(movePos+21) == Empty) return false
    if (padded(movePos-21) == Empty) return false

    padded(movePos) = move.color
    val paddedDia = Config.dia + 2
    val dst = Array.fill(paddedDia * paddedDia)(0)

    val blo, skip = mutable.Set[Int]()
    val lib = new MutableInt(0)
    loop(movePos, move.color, lib, skip, blo)
    blo foreach { x => dst(x) = lib.value }

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
        Seq(i+1, i-1, i+paddedDia, i-paddedDia).filterNot(shouldSkip.contains)
          .foreach {loop(_, checkColor, lib, shouldSkip, block)}
      }
    }
    dst(movePos) == 0
  }

}