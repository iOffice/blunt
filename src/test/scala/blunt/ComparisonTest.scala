import org.scalatest.{ FlatSpec, Matchers }

import blunt._
import blunt.Operator.Symbols._

import doobie.imports._
import cats._, cats.implicits._

class ComparisonTest extends FlatSpec with Matchers {
  lazy val qb = QueryBuilder[Post]

  def createConn = {
    val postA = Post(-1, "Post A", None, "Here's a post", 5)
    val postB = Post(-1, "Post B", None, "Here's another post", 4)
    val postC = Post(-1, "Post C", None, "Here's another post", 3)
    val postD = Post(-1, "Post D", None, "Here's another post", 2)
    val postE = Post(-1, "Post E", None, "Here's the last post", 1)
    for {
      _ <- qb.insert(postA, 'id).build.run
      _ <- qb.insert(postB, 'id).build.run
      _ <- qb.insert(postC, 'id).build.run
      _ <- qb.insert(postD, 'id).build.run
      _ <- qb.insert(postE, 'id).build.run
    } yield ()
  }

  "Greater than" should "return posts with more likes" in new InMemoryDB {
    val queryConn = for {
      _ <- createConn
      posts <- qb.select.where[>]('likes, 2).build.nel
    } yield posts
    val posts = queryConn.transact(transactor).unsafePerformIO
    posts.size shouldBe 3
    posts.forall(_.likes > 2) shouldBe true
  }

  "Greater than or equal to" should "return posts with more or equal likes" in new InMemoryDB {
    val queryConn = for {
      _ <- createConn
      posts <- qb.select.where[>=]('likes, 4).build.nel
    } yield posts
    val posts = queryConn.transact(transactor).unsafePerformIO
    posts.size shouldBe 2
    posts.forall(_.likes >= 4) shouldBe true
  }

  "Less than" should "return posts with less likes" in new InMemoryDB {
    val queryConn = for {
      _ <- createConn
      posts <- qb.select.where[<]('likes, 3).build.nel
    } yield posts
    val posts = queryConn.transact(transactor).unsafePerformIO
    posts.size shouldBe 2
    posts.forall(_.likes < 3) shouldBe true
  }

  "Less than or equal to" should "return posts with less or equal likes" in new InMemoryDB {
    val queryConn = for {
      _ <- createConn
      posts <- qb.select.where[<=]('likes, 1).build.nel
    } yield posts
    val posts = queryConn.transact(transactor).unsafePerformIO
    posts.size shouldBe 1
    posts.forall(_.likes <= 1) shouldBe true
  }
}
