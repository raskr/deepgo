import java.io.{BufferedWriter, FileWriter}

class FileManager {

  import java.io.File
  import scala.collection.mutable.ArrayBuffer

  private [this] val lock = new AnyRef

  private [this] final val buf = ArrayBuffer[String]()
  private [this] final val bufInvalid = ArrayBuffer[String]()
  private [this] final val bufTarget = ArrayBuffer[String]()

  private [this] final val maxRowInFile = 128
  private [this] var currentFileCount = 0
  private [this] var currentBufSize = 0

  // init
  val d = new File("file_out")
  if (d.exists && d.isDirectory) {
    d.delete()
    println("deleted existing file_out dir")
  }
  d.mkdir()

  final def commit(channel: String, target: Int, invalid: String) = {
    lock.synchronized{

      buf.append(channel)
      bufInvalid.append(invalid)
      bufTarget.append("" + target)
      currentBufSize += 1
      if (currentBufSize == maxRowInFile) {
        push()
        currentBufSize = 0
        buf.clear()
      }

    }
  }

  private [this] final def push() = {
    val bf = new BufferedWriter(new FileWriter(new File("file_out/" + currentFileCount), true))
    var i = 0
    while (i < maxRowInFile) {
      bf.write(buf(i))
      i += 1
    }
    bf.newLine()
    i = 0
    while (i < maxRowInFile) {
      bf.write(bufInvalid(i))
      i += 1
    }
    bf.newLine()
    bf.write(bufTarget.mkString(","))
    bf.newLine()
    bf.close()
    currentFileCount += 1
  }

}
