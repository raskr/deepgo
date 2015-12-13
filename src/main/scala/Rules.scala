object Rules {

  import scala.collection.mutable
  import Implicits._
  import Colors._

  def nextLifespans(x: Array[Int],
                    prevBoard: Array[Char],
                    curBoard: Array[Char]): Array[Int] =
  {
    val dst = (0 until Constants.all).toArray map { i =>
      val prev = prevBoard(i)
      val cur = curBoard(i)
      if (prev == cur && prev != Empty) x(i)+1 else 0
    }
    assert(dst.length/Constants.all == 1)
    dst
  }

  def groupSizes(in: Array[Char]) = {
    val padded = in.pad(Constants.dia, Constants.dia, 1, Outside)
    val dst = Array.fill(21*21)(0)

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
      if (shouldSkip(i) || dst(i) > 0) return
      shouldSkip.add(i)
      val col = padded(i)
      if (col == checkColor) { // same color
        block.add(i)
        Seq(i+1, i-1, i+21, i-21).filterNot(shouldSkip.contains)
          .foreach {loop(_, checkColor, shouldSkip, block)}
      }
    }

    dst.clip(21, 21, 1)
  }

  def findKo(move: Move, in: Array[Char]): Int = {
    val padded = in.pad(Constants.dia, Constants.dia, 1, Outside)
    val movePos = (move.y+1)*21 + (move.x+1)
    val around = Seq(movePos + 1, movePos - 1, movePos + 21, movePos - 21)
    val surrounded = around forall { padded(_) isOpponentOf move.color }
    val empty4 = around.filter { padded(_) == Empty }
    val ko21 = if (surrounded && empty4.size == 1) empty4.head else -1
    ko21.rectify(from = Constants.dia+2, to = Constants.dia)
  }

  def boardAfterCaptured(move: Move, curBoard: Array[Char]) = {
    // pad the input board for convenience
    val padded = curBoard.pad(Constants.dia, Constants.dia, 1, Outside)
    val movePos = (move.y+1)*21 + (move.x+1)
    // (true, false) ... (enemy, live)
    val booleans = Array.fill(21 * 21)(None: Option[Boolean])
    // Checked position in the recursive session.
    // This is indispensable.
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

  def liberties(board: Array[Char]): Array[Int] = {
    val padded = board.pad(Constants.dia, Constants.dia, 1, Outside)
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


}
