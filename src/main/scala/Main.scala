// sbt "run -m db -c w -s 4 -d path/to/sgfDir"
object Main {
  import Color._

  def main(args: Array[String]) = {
    val res          = Utils.parseArgs(args.toList)
    val dir          = res find (_._1.matches("-d||--dir"))
    val color        = res find (_._1.matches("-c||--color"))
    val mode         = res find (_._1.matches("-m||--mode"))
    val predStep     = res find (_._1.matches("-s||--pred-step")) // prediction step num

    (dir, color, mode, predStep) match {
      case (Some(d), Some(c), Some(m), Some(s)) if m._2 == "db" => // use sqlite3
        TopLevelTask.parseSGF(d._2, colorsFrom(c._2).map(new DB(_)), s._2.head-'0', Some(1))

      case (Some(d), _, _, Some(s)) => // test
        Config.DEBUG = true
        TopLevelTask.parseSGF(d._2, Seq(), s._2.head-'0', limit=Some(5))

      case (_, _, Some(m), Some(s))  =>
        Config.numPred = s._2.head-'0'
        GTP_CmdHandler.listenAndServe()

      case _ => throw new IllegalArgumentException("No valid arguments were given!!!")
    }
  }

}
