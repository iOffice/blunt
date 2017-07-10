import blunt._
import doobie.imports._

import java.util.UUID

case class Post(id: Int, title: String, subtitle: Option[String], text: String)
case class Comment(id: Int, postId: Int, text: String)

trait InMemoryDB {
  implicit val transactor = DriverManagerTransactor[IOLite](
    "org.h2.Driver",
    s"jdbc:h2:mem:${UUID.randomUUID.toString};DB_CLOSE_DELAY=-1;MODE=MSSQLServer",
    "ioffice",
    ""
  )

  lazy val createPosts = sql"""
    CREATE TABLE IF NOT EXISTS Posts (
      id          INT            NOT NULL IDENTITY(1, 1) PRIMARY KEY,
      title       VARCHAR(255)   NOT NULL,
      subtitle    VARCHAR(255),
      text        VARCHAR(255)   NOT NULL
    )
  """.update

  lazy val createComments = sql"""
    CREATE TABLE IF NOT EXISTS Comments (
      id          INT            NOT NULL IDENTITY(1, 1) PRIMARY KEY,
      postId      INT            NOT NULL,
      text        VARCHAR(255)   NOT NULL,
      FOREIGN KEY (postId) REFERENCES Posts(id)
    )
  """.update

  def createTables(implicit transactor: Transactor[IOLite]) = {
    val createConnection =
      for {
        _ <- createPosts.run
        _ <- createComments.run
      } yield ()
    createConnection.transact(transactor).unsafePerformIO
  }

  def lastId = sql"select lastval()".query[Int].unique

  createTables
}
