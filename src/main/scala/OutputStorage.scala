import java.io.{FileWriter, BufferedWriter, File}
import java.sql.DriverManager
import scala.collection.mutable.ArrayBuffer

/**
  * This represents OutputStorage where feature maps locate. python deep learning codes
  * use this output storage later.
  * for example, if we use sql database,
  *
  * column0... _id => usual primary key for good performance
  * column1... state => current feature map
  * column2... target => answer position that expert actually played
  */
sealed abstract class OutputStorage {
  // if color is `w`, feature maps for white player will saved in this output storage.
  val color: Char
  def commit(state: State, targets: Seq[Move])
  def close()
}

object DB {
  val dbName = "deepgo.db"
  val lock = new AnyRef
  private var closed = false
  private val conn = DriverManager.getConnection("jdbc:sqlite:" + dbName)

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
  DB.conn.prepareStatement(
    s"create table $color (_id Integer PRIMARY KEY AUTOINCREMENT, state TEXT, target Integer)").execute()

  println("created new table: " + color)

  private [this] val statement = DB.conn.prepareStatement(s"INSERT into $color (state, target) values (?, ?)")
  var currentRowCount = 0

  // You don't have to call statement#close and res#close as long as
  // conn#close() called certainly.
  def close() = DB.closeConnection()

  // statement.whatever() is "not" thread safe.
  // mutex is required.
  def commit(state: State, targets: Seq[Move]) = DB.lock.synchronized {
    state.toChannels(color=targets.head.color).foreach { ch =>
      currentRowCount += 1
      statement.setString(1, ch)
      statement.setString(2, targets.map(_.pos).mkString(","))
      statement.executeUpdate
      // save regularly
      if (currentRowCount % 5000000 == 0) DB.conn.commit()
    }
  }

}

