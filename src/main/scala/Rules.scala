object Rules {

  import scala.collection.mutable
  import Implicits._
  import Colors._

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

  def nextLifespans(x: Array[Int],
                    prevBoard: Array[Char],
                    curBoard: Array[Char]): Array[Int] =
  {
    val dst = Array.range(0, Config.all) map { i =>
      val prev = prevBoard(i)
      val cur = curBoard(i)
      if (prev == cur && prev != Empty) x(i)+1 else 0
    }
    assert(dst.length/Config.all == 1)
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

  def findKo(move: Move, in: Array[Char]): Int = {
    val padded = in.pad(Config.dia, Config.dia, 1, Outside)
    val paddedDia = Config.dia + 2
    val movePos = (move.y+1)*paddedDia + (move.x+1)
    val around = Seq(movePos + 1, movePos - 1, movePos + paddedDia, movePos - paddedDia)
    val surrounded = around forall { padded(_) isOpponentOf move.color }
    val empty4 = around.filter { padded(_) == Empty }
    val ko21 = if (surrounded && empty4.size == 1) empty4.head else -1
    ko21.rectify(from = paddedDia, to = Config.dia)
  }

  /**
    * @param move move
    * @param curBoard current board
    * @return (new board, captured Stone count)
    */
  def boardAfterCaptured(move: Move, curBoard: Array[Char]): Array[Char] = {
    val paddedDia = Config.dia + 2
    // pad the input board for convenience
    val padded = curBoard.pad(Config.dia, Config.dia, 1, Outside)
    val movePos = (move.y+1)*paddedDia + (move.x+1)
    // (true, false) ... (enemy, live)
    val booleans = Array.fill(paddedDia * paddedDia)(None: Option[Boolean])
    val shouldSkip = mutable.Set[Int]()

    // print
    println("\n\n\nbefore...")
    println(s"${move.color} (${if (move.color == White) 'X' else 'O'}) will play ...")
    curBoard.printState(Config.dia, Config.dia, Some(move), None)

    // 1. Reflect the move itself
    padded(movePos) = move.color
    booleans(movePos) = Some(true)

    killed = false
    // 2. Start inspection around the move-point and remove stones if possible
    Seq(movePos+1, movePos-1, movePos+paddedDia, movePos-paddedDia) foreach { i =>
      captureLoop(
        i = i,
        attackColor = move.color,
        board = padded,
        booleans = booleans,
        shouldSkip = shouldSkip
      )
    }

    val a = padded.clip(paddedDia, paddedDia, 1)
    println("after... ")
    if (killed) {
      println("killeddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd")
      killed = false
    }
    a.printState(Config.dia, Config.dia, None, None)
    a
  }

  def isSuicideMove(move: Move, curBoard: Array[Char]): Boolean = {
    def opponentOf(x: Char) = if (x == White) Black else White
    val paddedDia = Config.dia + 2
    val movePos = (move.y+1) * paddedDia + (move.x+1)
    val shouldSkip = mutable.Set[Int]()

    // pad the input board for convenience
    val padded = curBoard.pad(Config.dia, Config.dia, 1, Outside)

    // 1. Reflect the move itself
    padded(movePos) = move.color

    // 2. Start inspection about the move-point
    captureLoop(
      i = movePos,
      attackColor = opponentOf(move.color),
      board = padded,
      booleans = Array.fill(paddedDia * paddedDia)(None),
      shouldSkip = shouldSkip
    )

    padded(movePos) == Empty
  }

  /**
    * recursive func
    *
    * Called from ...
    *  boardAfterCaptured()
    *  isSuicideMove()
    *
    * @param i check index
    * @param attackColor literally...
    * @param board current board (should padded)
    * @param booleans the stone is a enemy or not)
    * @param shouldSkip prevent infinite loop
    * @return
    */
  def captureLoop(i: Int,
                  attackColor: Char,
                  board: Array[Char],
                  booleans: Array[Option[Boolean]],
                  shouldSkip:mutable.Set[Int]) : Boolean =
  {
    val paddedDia = Config.dia + 2
    val checkColor = board(i)
    if (booleans(i).isEmpty) {
      booleans(i) = Some(
        if (checkColor == attackColor) true
        else if (checkColor == Outside) true
        else if (checkColor == Empty) false
        else {
          val around = Seq(i+1, i-1, i+paddedDia, i-paddedDia)
            .filterNot{ shouldSkip.contains }
            .map(x => booleans(x).getOrElse {
              shouldSkip.add(i)
              captureLoop(x, attackColor, board, booleans, shouldSkip)
            })

          val shouldKill = around forall (_ == true)
          if (shouldKill) {
            killed =true
            board(i) = Empty
          }
          shouldKill
        })
    }
    booleans(i).get
  }

  var killed = false
}
