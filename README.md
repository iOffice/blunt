# blunt

<img align="right" src="https://cdn.rawgit.com/iOffice/blunt/5cdd7380/blunt_logo.svg" height="150px" style="padding-left: 20px"/>

**blunt** is a matter-of-fact query builder made with [doobie](https://github.com/tpolecat/doobie).
**blunt** provides a simple interface to map database schema to case classes, and an easy to use, typesafe way
of constructing queries. You get all the principled function programming that doobie provides, with an 
additional layer of composability and brevity.

## Example

``` scala
import doobie.imports._
import blunt._

val xa = DriverManagerTransactor[IOLite](
  // Set up your doobie transactor
  // ... 
)

//Define case classes to match your schema
case class Post(id: Int, title: String, subtitle: Option[String], text: String)
case class Comment(id: Int, postId: Int, text: String, likes: Int)

// Create a post and insert into db
val qb = QueryBuilder[Post]
val post = Post(-1, "A Good Post", None, "Here's a real good post")
qb
  .insert(post, 'id) // pass in the post and the id column to exclude
  .build
  .run
  .transact(xa)
  .unsafePerformIO

val postId = // Get the post id with doobie and your database's preferred way

// Create a comment and insert into db
QueryBuilder[Comment]
  .insert(Comment(-1, postId, "This post is good", 5), 'id)
  .build.run.transact(xa)
  .unsafePerformIO

// Select the posts that have comments with 
// greater than or equal to 5 likes
qb.select
  .join[Comment]
  .on('id, 'postId)
  .project[Comment]
  .where[>=]('likes, 5)
  .build
  .nel
  .transact(xa)
  .unsafePerformIO
// res1: NonEmptyList[(Post, Comment)] = ...
```

## Custom Table Names

Blunt will attempt to dervive a `Queryable` instance for you, by using a pluralized
version of the case class name as the table name. If this convention doesn't work for you,
or the pluralization is incorrect, you can provide your own `Queryable` instance:

``` scala
case class Medium(id: Int, name: String)

object Medium {
  implicit val queryable = new Queryable[Medium] {
    val columns = Seq(fr"id", fr"name") // The order of these should match your case class fields
    val table = Fragment.const("Media")
  }
}
```
