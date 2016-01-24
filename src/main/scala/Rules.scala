object Rules {

  import scala.collection.mutable
  import Implicits._
  import Color._

  // dead position is the position that liberty is 0
  def deadPositions(move: Move, board: Array[Char]): mutable.Set[Int] = {
    val padded = board.pad(Config.dia, Config.dia, 1, Outside)
    val dst = mutable.Set[Int]()
    val movePos = (move.y+1)*21 + (move.x+1)
    val checked = mutable.Set[Int]()

    Array(movePos+1, movePos-1, movePos+21, movePos-21) foreach { idx =>
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
        Array(i+1, i-1, i+21, i-21).filterNot(shouldSkip.contains)
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
        Array(i+1, i-1, i+21, i-21).filterNot(shouldSkip.contains)
          .foreach {loop(_, checkColor, lib, shouldSkip, block)}
      }
    }
    dst.clip(paddedDia, paddedDia, 1)
  }

  // replacing these ugly codes with loop is so difficult maybe...
  def genInitialBoard(ha: Option[Int]) = {
    val init = Utils.empties(Config.all)
    ha.foreach { h =>
      val ur = 19 * 4 - 3 - 1
      val dl = 19 * 15 + 4 - 1
      val dr = 19 * 19 - 19 * 3 - 3 - 1
      val ul = 19 * 3 + 4 - 1
      val c = 19 * 9 + 10 - 1
      val l = 19 * 9 + 4 - 1
      val r = 19 * 10 - 3 - 1
      val u = 19 * 3 + 10 - 1
      val d = 19 * 19 - 19 * 4 + 10 - 1

      if (h == 2) {
        init(ur) = Color.Black
        init(dl) = Color.Black
      } else if (h == 3) {
        init(ur) = Color.Black
        init(dl) = Color.Black
        init(dr) = Color.Black
      } else if (h == 4) {
        init(ur) = Color.Black
        init(dl) = Color.Black
        init(dr) = Color.Black
        init(ul) = Color.Black
      } else if (h == 5) {
        init(ur) = Color.Black
        init(dl) = Color.Black
        init(dr) = Color.Black
        init(ul) = Color.Black
        init(c) = Color.Black
      } else if (h == 6) {
        init(ur) = Color.Black
        init(dl) = Color.Black
        init(dr) = Color.Black
        init(ul) = Color.Black
        init(l) = Color.Black
        init(r) = Color.Black
      } else if (h == 7) {
        init(ur) = Color.Black
        init(dl) = Color.Black
        init(dr) = Color.Black
        init(ul) = Color.Black
        init(l) = Color.Black
        init(r) = Color.Black
        init(c) = Color.Black
      } else if (h == 8) {
        init(ur) = Color.Black
        init(dl) = Color.Black
        init(dr) = Color.Black
        init(ul) = Color.Black
        init(l) = Color.Black
        init(r) = Color.Black
        init(u) = Color.Black
        init(d) = Color.Black
      } else if (h == 9) {
        init(ur) = Color.Black
        init(dl) = Color.Black
        init(dr) = Color.Black
        init(ul) = Color.Black
        init(l) = Color.Black
        init(r) = Color.Black
        init(u) = Color.Black
        init(d) = Color.Black
        init(c) = Color.Black
      }
    }
    init
  }

  def boardAfterCaptured(move: Move, board: Array[Char]): Array[Char] = {
    val dst = board.clone()
    dst(move.pos) = move.color
    deadPositions(move, dst).foreach{ dst(_) = Empty }
    val a = Utils.checkBoardLegality(dst)
    println("before...")
    board.printState(19, 19, Some(move), None)
    println("after...")
    dst.printState(19, 19, None, None)
    if (!a) {
      throw new RuntimeException
    }
    dst
  }

  // 1ch
  def nextLifespans(curLifespan: Array[Int],
                    prevBoard: Array[Char],
                    curBoard: Array[Char]): Array[Int] =
  {
    val dst = Array.range(0, Config.all) map { i =>
      val (prev, cur) = (prevBoard(i), curBoard(i))
      if (prev == cur && prev != Empty) curLifespan(i)+1 else 0
    }
    dst
  }

  def groupSizes(in: Array[Char]): Array[Int] = {
    val padded = in.pad(Config.dia, Config.dia, 1, Outside)
    val dst = Array.fill(Config.padAll)(-1)

    (0 until Config.padAll) foreach { idx =>
      if (padded(idx).isStone) {
        val group = mutable.Set[Int]()
        loop(idx, padded(idx), mutable.Set[Int](), group)
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
        Array(i+1, i-1, i+21, i-21).filterNot(shouldSkip.contains)
          .foreach {loop(_, checkColor, shouldSkip, block)}
      }
    }

    dst.clip(Config.padDia, Config.padDia, 1).map(x => if (x == -1) 0 else x)
  }

  def findKo(move: Move, prevBoard: Array[Char], newBoard: Array[Char]): Int = {
    val paddedPrev = prevBoard.pad(Config.dia, Config.dia, 1, Outside)
    val paddedNew = newBoard.pad(Config.dia, Config.dia, 1, Outside)
    val movePos = (move.y+1)*Config.padDia + (move.x+1)
    val around = Array(movePos+1, movePos-1, movePos+Config.padDia, movePos-Config.padDia)
    val surrounded = around forall { paddedPrev(_) isOpponentOf move.color }
    val empty4 = around.filter { paddedNew(_) == Empty }
    val ko21 = if (surrounded && empty4.length == 1) empty4.head else -1
    ko21.rectify(from = Config.padDia, to = Config.dia)
  }

  def isSuicideMove(move: Move, board: Array[Char]): Boolean = {
    val padded = board.pad(Config.dia, Config.dia, 1, Outside)
    val movePos = (move.y+1)*21 + (move.x+1)

    if (padded(movePos+1) == Empty) return false
    if (padded(movePos-1) == Empty) return false
    if (padded(movePos+21) == Empty) return false
    if (padded(movePos-21) == Empty) return false

    padded(movePos) = move.color

    val blo = mutable.Set[Int]()
    val lib = new MutableInt(0)
    loop(movePos, move.color, lib, mutable.Set(), blo)

    // recursive func
    def loop(i: Int, checkColor: Char, lib: MutableInt,
             shouldSkip: mutable.Set[Int], block: mutable.Set[Int]): Unit =
    {
      if (shouldSkip(i)) return
      shouldSkip.add(i)
      val col = padded(i)
      if (col == Empty) lib += 1
      else if (col == checkColor) { // same color
        block.add(i)
        Array(i+1, i-1, i+21, i-21).filterNot(shouldSkip.contains)
          .foreach {loop(_, checkColor, lib, shouldSkip, block)}
      }
    }

    lib.value == 0
  }

}