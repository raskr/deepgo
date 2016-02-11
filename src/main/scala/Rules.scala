object Rules {

  import scala.collection.mutable
  import Implicits._
  import Color._

  // You should reflect the move to board before call this function
  def positionsCapturedBy(move: Move, board: Array[Char]): mutable.Set[Int] = {
    val padded = board.pad(Config.dia, Config.dia, 1, Outside)
    val dst = mutable.Set[Int]()
    val movePos = (move.y+1)*21 + (move.x+1)
    val checked = mutable.Set[Int]()

    Array(movePos+1, movePos-1, movePos+21, movePos-21) foreach { idx =>
      val a = padded(idx)
      if (a.isStone && a.isOpponentOf(move.color)) {
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
    positionsCapturedBy(move, dst).foreach{ dst(_) = Empty }
    //if (!Utils.checkBoardLegality(dst)) {
    //  println("bad before")
    //  board.printState(19, 19, Some(move), None)
    //  println("bad after ")
    //  board.printState(19, 19, None, None)
    //}
    dst
  }

  // 1ch
  def nextHistory(curLifespan: Array[Int],
                  prevBoard: Array[Char],
                  curBoard: Array[Char]): Array[Int] =
  {
    val dst = Array.range(0, Config.all) map { i =>
      val (prev, cur) = (prevBoard(i), curBoard(i))
      if (prev == cur && prev != Empty) curLifespan(i)+1 else 0
    }
    dst
  }

  // group size = a.k.a. `Ren`
  def groupSizes(in: Array[Char]): Array[Int] = {
    val padded = in.pad(Config.dia, Config.dia, 1, Outside)
    val dst = Array.fill(Config.padAll)(-1)

    var idx = 0
    val all = Config.padAll
    while (idx < all) {
      if (padded(idx).isStone) {
        val group = mutable.Set[Int]()
        loop(idx, padded(idx), mutable.Set[Int](), group)
        val groupSize = group.size
        group foreach { x => dst(x) = groupSize }
      }
      idx += 1
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

  // separated position version
  // tested
  def borderPositions1(dia: Int):
  (mutable.Set[Int], mutable.Set[Int], mutable.Set[Int], mutable.Set[Int]) = {

    val up = mutable.Set[Int]()
    val down = mutable.Set[Int]()
    val left = mutable.Set[Int]()
    val right = mutable.Set[Int]()

    // first row
    var i = 0
    while (i < dia) {
      up.add(i)
      i += 1
    }

    // last row
    val all = dia * dia
    (all - dia until all) foreach { i => down.add(i) }


    { // left side
    var (i, n) = (0, 0)
      while (n <= dia-1) {
        left.add(i)
        i += dia
        n += 1
      }
    }

    { // right side
    var (i, n) = (dia-1, 0)
      while (n <= dia-1) {
        right.add(i)
        i += dia
        n += 1
      }
    }

    (up, right, down, left)
  }

  def borderPositions(dia: Int): mutable.Set[Int] = {
    val set = mutable.Set[Int]()

    // first row
    var i = 0
    while (i < dia) {
      set.add(i)
      i += 1
    }

    // last row
    val all = dia * dia
    (all - dia until all) foreach { i => set.add(i) }


    { // left side
    var (i, n) = (dia, 1)
      while (n <= dia-2) {
        set.add(i)
        i += dia
        n += 1
      }
    }

    { // right side
    var (i, n) = (2*dia-1, 1)
      while (n <= dia-2) {
        set.add(i)
        i += dia
        n += 1
      }
    }

    set
  }

  def fillEmptyNeighbor(idx: Int, fillColor: Char, paddedBoard: Array[Char]): Array[Char] = {

    // flags stand for the whether a edge was touched or not
    var (up, right, down, left) = (false, false, false, false)
    val (ups, rights, downs, lefts) = borderPositions1(Config.dia + 2)
    val editable =  paddedBoard.clone()

    def touched = Array(up, right, down, left).count(_ == true)

    loop(idx)

    def loop(i: Int): Unit = {
      // update
      if (editable(i) == Empty) editable(i) = fillColor
      val around = Array(i+1, i-1, i+21, i-21)
      around.foreach{ x =>
        // check which edge i is the located in
        if      (ups.contains(x)) up = true
        else if (rights.contains(x)) right = true
        else if (downs.contains(x)) down = true
        else if (lefts.contains(x)) left = true
      }
      if (touched < 3) around.filter{ editable(_) == Empty } foreach loop
    }

    if (touched >= 3) {
      // I don't fill. return the original argument because
      // modification was mistake.
      paddedBoard.clone()
    } else { // normal
      editable
    }

  }

  def isSuicideMove(move: Move, board: Array[Char]): Boolean = {
    val movePos = (move.y+1)*21 + (move.x+1)
    var padded = board.pad(Config.dia, Config.dia, 1, Outside)

    padded(movePos) = move.color
    padded = fillEmptyNeighbor(movePos, move.color, padded)

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
    lib.value == 0 && {
      val b = board.clone()
      b(move.pos) = move.color
      positionsCapturedBy(move, b).isEmpty
    }
  }

}
