package blunt

import doobie.imports._

abstract class Repo[T : Composite : Queryable] {
  val qb = QueryBuilder[T]

  def findByIdQuery(idVal: Int): Query0[T]

  def findById(id: Int) =
    findByIdQuery(id).option

  lazy val all = qb.select.build
}
