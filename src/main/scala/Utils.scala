import java.io.File

import scala.collection.mutable
import scala.util.Random

object Utils {

  def zeros(size: Int): Array[Char] = Array.fill(size)('0')
  def ones(size: Int): Array[Char] = Array.fill(size)('1')

  def listFilesIn(dir: String, limit: Option[Int], extension: Option[String]) = {
    val d = new File(dir)
    val dst =
      if (d.exists && d.isDirectory) {
        val a = d.listFiles.toSeq
        limit match {
          case Some(x) if a.isEmpty =>
            println("There are not sgf files in sgf dir")
            Seq[File]()
          case Some(x) if a.size > x => a.take(x)
          case _ => a
        }
      } else {
        d.mkdir()
        println("Created new directory. Put files in it and then run again.")
        Seq[File]()
      }
    extension match {
      case Some(a) => dst.filter(_.getName endsWith a)
      case _  => dst
    }
  }

  // internal test
  //  def testGame(states: List[State], moves: List[Move]) = {
//    (states.init, moves, states.tail).zipped map {
//      (oldState, move, newState) => assert(oldState.next(move, "", print=false) == newState)
//    }
//  }

  def intArr2charArr(x: Array[Int]) = x.map{ a => if (a > 9) '9' else ('0'+a).toChar }
  def charArr2intArr(x: Array[Char]) = x.map{ _.asDigit }

  def zipMap[T, U, V](list1: Seq[T], list2: Seq[U])(f: Function2[T, U, V]) = {
    val iter1 = list1.iterator
    val iter2 = list2.iterator

    var dst = List[V]()
    while (iter1.hasNext) dst = f(iter1.next(), iter2.next()) :: dst

    dst.reverse.toSeq
  }

  def zipEach[T, U](list1: Seq[T], list2: Seq[U])(f: Function2[T, U, Unit]) = {
    val iter1 = list1.iterator
    val iter2 = list2.iterator
    while (iter1.hasNext) f(iter1.next(), iter2.next())
  }

  def borderPositions(dia: Int) = {
    val set = mutable.Set[Int]()

    // first row
    (0 until dia) foreach set.add

    // last row
    val all = dia * dia
    (all - dia until all) foreach set.add


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

  def createRandomBoard: Array[Char] = {
    import Colors._
    val rand = new Random
    def next = rand.nextInt(3)
    (0 until 361).map{ i =>
      next match {
        case 0 => Empty
        case 1 => White
        case 2 => Black
      }
    }.toArray
  }

  def execCmd(cmd: String): String = {
    import scala.sys.process._
    Process(cmd).!!
  }

  // pad the array around with the provide element
  def pad(in: Array[Char], row: Int, col: Int, padSize: Int, padElem: Char) = {
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

  def clip(in: Array[Char], row: Int, col: Int, borderWidth: Int) = {
    assert(in.length == row * col)
    val dstRow = row - borderWidth * 2
    val dstCol = col - borderWidth * 2
    val dst = Array.fill(dstRow * dstCol)(' ')
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

  def clip(x: Array[Int], row: Int, col: Int, borderWidth: Int) = {
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


  //  def pad[@specialized(Int, Char) A](in: Array[A],
  //                                     row: Int,
  //                                     col: Int,
  //                                     padSize: Int,
  //                                     padElem: A): Array[A] = {
  //    assert(in.length == row * col)
  //    val allNum = row * col
  //
  //    def fill(n: Int) = Array.fill(n)(padElem)
  //    def chunk: Array[A] = fill((col + padSize * 2) * padSize)
  //
  //    // 1 (chunk + 2)
  //    var dst = Array.concat(chunk, fill(padSize))
  //
  //    var i = 0
  //    while (i <= allNum - col * 2) {
  //      // slice one row
  //      val a = in.slice(i, i + col)
  //      val b = Array.concat(a, fill(padSize * 2))
  //      dst = Array.concat(dst, b)
  //      i += col
  //    }
  //    dst = dst ++ (in.slice(i, i + col) ++ (fill(padSize) ++ chunk.clone()))
  //    dst
  //  }

  // clip the array with the provided border
  //  def clip1[@specialized(Int, Char) A](in:Array[A],
  //                                      row: Int,
  //                                      col: Int,
  //                                      borderWidth: Int): Array[A] = {
  //    assert(in.length == row * col)
  //    val dstRow = row - borderWidth * 2
  //    val dstCol = col - borderWidth * 2
  //    val dst = new Array[A](dstRow * dstCol)
  //    var i, j = 0
  //    while (i < dstRow) {
  //      j = 0
  //      while (j < dstCol) {
  //        dst(dstCol*i+ j) = in((i+borderWidth) * col + (j + borderWidth))
  //        j += 1
  //      }
  //      i += 1
  //    }
  //    dst
  //  }

  // clip the array with the provided border
}
