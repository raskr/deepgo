import java.io.File
import java.sql.DriverManager

object DB { val DbName = "deepgo.db" }
class DB {

  { // init
    val f = new File(DB.DbName)
    if (f.exists) { f.delete(); println("deleted existing db") }
    println("create new sqlite database ...")
  }

  private [this] val conn = DriverManager.getConnection("jdbc:sqlite:" + DB.DbName)
  conn.setAutoCommit(false)
  conn.prepareStatement("create table white (" +
    "_id Integer PRIMARY KEY AUTOINCREMENT," +
    "state TEXT," +
    "target SMALLINT," +
    "invalid TEXT)").execute()

  private [this] val statement = conn.prepareStatement("insert into white (state, target, invalid) values (?, ?, ?)")

  def insert(state: State, target: Move) = {
    statement.setString(1, state.toChannels)
    statement.setInt(2, target.pos)
    statement.setString(3, state.invalidChannel)
    statement.executeUpdate
  }

  def close() = { statement.close();  conn.close() }
  def save() = conn.commit()
}
