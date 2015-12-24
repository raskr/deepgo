import java.io.{FileWriter, BufferedWriter, File}
import java.sql.DriverManager
import scala.collection.mutable.ArrayBuffer

sealed abstract class OutputStorage {
  def commit(state: State, target: Move)
  def close()
}

class DB(color: String) extends OutputStorage {
  private [this] final val dbName = "deepgo.db"
  private [this] final val conn = DriverManager.getConnection("jdbc:sqlite:" + dbName)
  private [this] final val statement = conn.prepareStatement(s"insert into $color (state, target, invalid) values (?, ?, ?)")
  private [this] final val f = new File(dbName)
  private [this] var currentRowCount = 0

  if (f.exists) { f.delete(); println("deleted existing db") }
  println("create new sqlite database ...")

  conn.setAutoCommit(false)
  conn.prepareStatement("create table white (" +
    "_id Integer PRIMARY KEY AUTOINCREMENT," +
    "state TEXT," +
    "target SMALLINT," +
    "invalid TEXT)").execute()

  final def close() = { conn.commit(); statement.close();  conn.close() }
  final def commit(state: State, target: Move) = {
    currentRowCount += 1
    statement.setString(1, state.toChannels)
    statement.setInt(2, target.pos)
    statement.setString(3, state.invalidChannel)
    statement.executeUpdate
    if (currentRowCount % 50000 == 0) conn.commit()
  }
}

class Files(color: String) extends OutputStorage {
  private [this] final val lock = new AnyRef
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

  final def commit(state: State, target: Move) = lock.synchronized {
    buf.append(state.toChannels)
    bufInvalid.append(state.invalidChannel)
    bufTarget.append("" + target.pos)
    currentBufSize += 1
    if (currentBufSize == maxRowInFile) {
      push()
      currentBufSize = 0
      buf.clear()
    }
  }

  final def close() = {}

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

