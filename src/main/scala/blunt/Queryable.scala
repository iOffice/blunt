package blunt

import doobie.imports._
import cats._, cats.implicits._
import shapeless._
import shapeless.ops.record._
import shapeless.ops.hlist._

import org.atteo.evo.inflector.English

import scala.reflect.runtime.universe.TypeTag

/*
 * The `Queryable` type class provides all the info needed to build
 * queries for a given table. A generic derivation is available for
 * case classes, but if you need a table name or field names, you must
 * derive your own instance.
 */
abstract class Queryable[T : Composite] {
  def columns: Seq[Fragment]
  def table: Fragment

  def allColumns = columns.toList.intercalate(fr",")
}

object Queryable {
  def apply[T : Composite](implicit q: Queryable[T]) = q

  /*
   * Splits a `"WordLikeThis"` into a list of `["word", "like", "this"]`
   * with the purpose of pluralizing the last word of case classes.
   */
  def splitCamelCase(words: String): Seq[String] = {
    val indices = words.zipWithIndex.filter(_._1.isUpper).map(_._2)
    val (rest, result) = indices.foldRight((words, List.empty[String])) {
      case (index, (s, result)) =>
        val (rest, split) = s.splitAt(index)
        (rest, split :: result)
    }
    (rest :: result).filter(_.nonEmpty)
  }

  /*
   * The generic queryable instance attempts to derive the table name
   * by pluralizing the case class name, and treating all case class
   * fields as the column names. 
   */
  implicit def genericQueryable[T : Composite, H <: HList, K <: HList](
    implicit generic: LabelledGeneric.Aux[T, H],
    keys: Keys.Aux[H, K],
    keysList: ToTraversable.Aux[K, List, Symbol],
    typeTag: TypeTag[T]
  ): Queryable[T] = {
    val columnsFrag = keys.apply.toList.map(s => Fragment.const(s.name))
    val typeName = splitCamelCase(typeTag.tpe.toString)
    val pluralized = typeName.dropRight(1) ++ 
      typeName.lastOption.map(English.plural(_)).toList
    val tableFrag = Fragment.const(pluralized.mkString)
    new Queryable[T] {
      val columns = columnsFrag
      val table = tableFrag
    }
  }
}
