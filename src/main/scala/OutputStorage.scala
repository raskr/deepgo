import java.io.{FileWriter, BufferedWriter, File}
import java.sql.DriverManager
import scala.collection.mutable.ArrayBuffer

sealed abstract class OutputStorage {
  val color: Char
  def commit(state: State, targets: Seq[Move])
  def close()
}

object DB {
  val dbName = "deepgo.db"
  val lock = new AnyRef
  var closed = false
  var currentRowCount = 0
  private lazy val conn = DriverManager.getConnection("jdbc:sqlite:" + dbName)

  def closeConnection() = lock.synchronized {
    if (!closed) {
      closed = true
      conn.commit()
      conn.close()
    }
  }
}

final class DB(val color: Char) extends OutputStorage {

  DB.conn.setAutoCommit(false)
  DB.conn.prepareStatement("DROP TABLE IF EXISTS " + color).execute()
  DB.conn.prepareStatement(s"create table $color (" +
    "_id Integer PRIMARY KEY AUTOINCREMENT," +
    "state TEXT," +
    "target Integer," +
    "invalid TEXT)").execute()

  println("created new table: " + color)

  private [this] val statement = DB.conn.prepareStatement(s"INSERT into $color (state, target, invalid) values (?, ?, ?)")

  // You don't have to call statement#close and res#close as long as
  // conn#close() called certainly.
  def close() = DB.closeConnection()

  // statement.whatever() is "not" thread safe.
  // mutex is required.
  def commit(state: State, targets: Seq[Move]) = DB.lock.synchronized {
    state.toChannels.foreach { ch =>
      DB.currentRowCount += 1
      statement.setString(1, ch)
      statement.setString(2, targets.map(_.pos).mkString(","))
      statement.setString(3, state.invalidChannel.mkString)
      statement.executeUpdate
      // save regularly
      if (DB.currentRowCount % 5000000 == 0) DB.conn.commit()
    }
  }

}

final class Files(val color: Char) extends OutputStorage {
  private [this] val lock = new AnyRef
  private [this] val buf = ArrayBuffer[String]()
  private [this] val bufInvalid = ArrayBuffer[String]()
  private [this] val bufTarget = ArrayBuffer[String]()
  private [this] val maxRowInFile = 128
  private [this] var currentFileCount = 0
  private [this] var currentBufSize = 0

  // init
  val d = new File(s"file_out_$color")
  if (d.exists && d.isDirectory) {
    d.delete()
    println("deleted existing file_out dir")
  }
  d.mkdir()

  def commit(state: State, targets: Seq[Move]) = DB.lock.synchronized {
    state.toChannels.foreach { ch =>
      buf.append(ch)
      bufInvalid.append(state.invalidChannel.mkString)
      bufTarget.append("" + targets.map(_.pos).mkString(","))
      currentBufSize += 1
      if (currentBufSize == maxRowInFile) {
        push()
        currentBufSize = 0
        buf.clear()
      }
    }
  }

  def close() = {}

  private [this] def push() = {
    val bf = new BufferedWriter(new FileWriter(new File(s"file_out_$color/" + currentFileCount), true))
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

