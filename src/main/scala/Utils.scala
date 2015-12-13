import java.io.{InputStreamReader, BufferedReader, File}

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
    if (list1.isEmpty)throw new RuntimeException("list1.isEmpty")
    if ( list1.size != list2.size) throw new RuntimeException("list1.size != list2.size")

    val iter1 = list1.iterator
    val iter2 = list2.iterator

    var dst = List[V]()
    while (iter1.hasNext) dst = f(iter1.next(), iter2.next()) :: dst

    dst.reverse.toSeq
  }

  def zipEach[T, U](list1: Seq[T], list2: Seq[U])(f: Function2[T, U, Unit]) = {
    if (list1.isEmpty ) throw new RuntimeException("list1.isEmpty")
    if ( list1.size != list2.size) throw new RuntimeException("list1.size != list2.size")

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


  // tested
  // (x, y)
  // top-left is 0. pos is based on this
  def num2coordinate(num: Int): (Int, Int) = {
    val y = num / Constants.dia
    val x = num - (Constants.dia * y)
    (x, y)
  }
  // 0 => a
  // 1 => b
  def num2alpha(num: Int): Char = ('a' + num).toChar

}
