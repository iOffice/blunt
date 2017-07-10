import org.scalatest.{ FlatSpec, Matchers }

import blunt._
import doobie.imports._
import cats._, cats.implicits._

class LoggingTest extends FlatSpec with Matchers {
  class LogHappened extends Exception

  "logging" should "happen" in new InMemoryDB {
    implicit val logHandler = LogHandler(_ => throw new LogHappened)
    val qb = QueryBuilder[Post].select
    assertThrows[LogHappened] {
      qb.build.option.transact(transactor).unsafePerformIO
    }
  }
}
