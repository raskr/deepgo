object Border1Test extends App {

  import Implicits._

  val dia = 21
  val a = Array.fill(dia*dia)(0)
  val (c, d, e, f) = Rules.borderPositions1(dia)
  c.foreach{ a(_) = 1 }
//  d.foreach{ a(_) = 1 }
//  e.foreach{ a(_) = 1 }
//  f.foreach{ a(_) = 1 }
  a.printState(dia, dia)

  // channel
  val b = Utils.createRandomBoard
  b.printState(19, 19, None, None)
  b.toBorderChannel.toCharArray.printState(19, 19, None, None)

}
