import java.io.{FileWriter, BufferedWriter, File}
import java.sql.DriverManager
import scala.collection.mutable.ArrayBuffer

sealed abstract class OutputStorage {
  def commit(channel: String, target: Int, invalid: String)
  def close(): Boolean
}

class Files extends OutputStorage {
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

  final def commit(channel: String, target: Int, invalid: String) = lock.synchronized {
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

  final def close() = true

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

class DB extends OutputStorage {

  private [this] final val conn = DriverManager.getConnection("jdbc:sqlite:" + DB.DbName)
  private [this] final val statement = conn.prepareStatement("insert into white (state, target, invalid) values (?, ?, ?)")
  private [this] final val f = new File(DB.DbName)

  if (f.exists) { f.delete(); println("deleted existing db") }
  println("create new sqlite database ...")

  conn.setAutoCommit(false)
  conn.prepareStatement("create table white (" +
    "_id Integer PRIMARY KEY AUTOINCREMENT," +
    "state TEXT," +
    "target SMALLINT," +
    "invalid TEXT)").execute()


  final def commit(state: String, target: Int, invalid: String) = {
    statement.setString(1, state)
    statement.setInt(2, target)
    statement.setString(3, invalid)
    statement.executeUpdate
  }

  final def close() = { conn.commit(); statement.close();  conn.close() }
}

