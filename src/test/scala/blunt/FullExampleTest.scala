import org.scalatest.{ FlatSpec, Matchers }

import blunt._
import doobie.imports._
import cats._, cats.implicits._

class FullExampleTest extends FlatSpec with Matchers {
  lazy val qb = QueryBuilder[Post]

  "Insert" should "create a post in the database" in new InMemoryDB{
    val post = Post(-1, "The Good Stuff", None, "Here's a list of the good stuff: food")
    val postConnection = for {
      _ <- qb.insert(post, 'id).build.run
      id <- lastId
      post <- qb.select.where('id, id).build.option
    } yield post
    val dbPost = postConnection.transact(transactor).unsafePerformIO
    dbPost.map(_.title) shouldBe Some("The Good Stuff")
  }

  "Select" should "find a post with a given title" in new InMemoryDB {
    val post = Post(-1, "Things I Hate", Some("Aka The Bad Stuff"), "I really don't like cleaning")
    val postConnection = for {
      _ <- qb.insert(post, 'id).build.run
      post <- qb.select.where('title, "Things I Hate").build.option
    } yield post
    val dbPost = postConnection.transact(transactor).unsafePerformIO
    dbPost.map(_.title) shouldBe Some("Things I Hate")
  }

  "Join" should "return a post with its comments" in new InMemoryDB {
    val post = Post(-1, "A Popular Post", None, "Everyone's commenting")
    def makeComment(postId: Int) = Comment(-1, postId, "This post sux")
    val commentQb = QueryBuilder[Comment]
    val commentsConnection = for {
      _ <- qb.insert(post, 'id).build.run
      id <- lastId
      _ <- commentQb.insert(makeComment(id), 'id).build.run
      _ <- commentQb.insert(makeComment(id), 'id).build.run
      _ <- commentQb.insert(makeComment(id), 'id).build.run
      commentsWithPost <- qb.select.join[Comment].on('id, 'postId).build.nel
    } yield commentsWithPost
    val comments = commentsConnection.transact(transactor).unsafePerformIO
    comments.size shouldBe 3
  }

  it should "work in conjuction with where" in new InMemoryDB {
    val post = Post(-1, "A Popular Post", None, "Everyone's commenting")
    val commentQb = QueryBuilder[Comment]
    val commentsConnection = for {
      _ <- qb.insert(post, 'id).build.run
      id <- lastId
      _ <- commentQb.insert(Comment(-1, id, "This post sux"), 'id).build.run
      _ <- commentQb.insert(Comment(-1, id, "This post sux"), 'id).build.run
      _ <- commentQb.insert(Comment(-1, id, "A positive comment", 5), 'id).build.run
      commentsWithPost <- qb.select.join[Comment].on('id, 'postId).project[Comment].where('likes, 5).build.nel
    } yield commentsWithPost
    val comments = commentsConnection.transact(transactor).unsafePerformIO
    comments.size shouldBe 1
  }

  "Update" should "fix a typo" in new InMemoryDB {
    val post = Post(-1, "A Wrong Post", None, "Whoops I mad a typeo")
    val postConn = for {
      _ <- qb.insert(post, 'id).build.run
      id <- lastId
      _ <- qb.update('text, "Whoops I made a typo").where('id, id).build.run
      post <- qb.select.where('id, id).build.option
    } yield post
    val dbPost = postConn.transact(transactor).unsafePerformIO
    dbPost.map(_.text) shouldBe Some("Whoops I made a typo")
  }

  "Delete" should "remove a post" in new InMemoryDB {
    val post = Post(-1, "A Bad Post", None, "Plz delete me")
    val postConn = for {
      _ <- qb.insert(post, 'id).build.run
      id <- lastId
      _ <- qb.delete.where('id, id).build.run
      post <- qb.select.where('id, id).build.option
    } yield post
    val dbPost = postConn.transact(transactor).unsafePerformIO
    dbPost shouldBe None
  }
}
