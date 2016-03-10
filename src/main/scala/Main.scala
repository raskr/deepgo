// sbt "run -m db -c w -s 3 -d path/to/sgfDir"
object Main {
  import Color._

  def main(args: Array[String]) = {
    val res = UtilMethods.parseArgs(args.toList)

    // sgf files (training data) dir
    val dir = res find (_._1.matches("-d||--dir"))

    // your color ... ``w`` or ``b`` or ``wb``. This is also used for training.
    // If ``w``, database for white player will be made. Else if ``wb``, two database will be made.
    val color = res find (_._1.matches("-c||--color"))

    // mode ... ``db`` or ``test``. ``db`` mean production and create sqlite database containing feature map data.
    val mode = res find (_._1.matches("-m||--mode"))

    // prediction step num like ``3``. Multiple prediction improve strength according to
    // paper from Facebook (http://arxiv.org/pdf/1511.06410v1.pdf). I don't know whether Google AlphaGo using this algo.
    val predStep = res find (_._1.matches("-s||--pred-step"))

    (dir, color, mode, predStep) match {
      case (Some(d), Some(c), Some(m), Some(s)) if m._2 == "db" => // use sqlite3
        TopLevelTask.parseSGF(d._2, colorsFrom(c._2).map(new DB(_)), s._2.head-'0', None)

      case (Some(d), _, Some(m), Some(s)) if m._2 == "test" => // test
        Config.DEBUG = true
        TopLevelTask.parseSGF(d._2, Seq(), s._2.head-'0', limit=Some(5))

      case (_, _, Some(m), Some(s))  =>
        Config.numPred = s._2.head-'0'
        GTPCmdHandler.listenAndServe()

      case _ => throw new IllegalArgumentException("No valid arguments were given!!!")
    }
  }

}
