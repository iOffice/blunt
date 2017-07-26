package blunt

import doobie.imports._
import cats.implicits._

sealed trait Operator

case class lt() extends Operator 
case class gt() extends Operator 
case class gte() extends Operator
case class lte() extends Operator
case class eql() extends Operator

trait ToSql[O <: Operator] {
  def sql: Fragment
}

object Operator {
  implicit val ltSql = new ToSql[lt] { val sql = fr"<" }
  implicit val lteSql = new ToSql[lte] { val sql = fr"<=" }
  implicit val gtSql = new ToSql[gt] { val sql = fr">" }
  implicit val gteSql = new ToSql[gte] { val sql = fr">=" }
  implicit val eqlSql = new ToSql[eql] { val sql = fr"=" }

  object Symbols {
    type < = lt
    type <= = lte
    type > = gt
    type >= = gte
    type `=` = eql
  }
}
