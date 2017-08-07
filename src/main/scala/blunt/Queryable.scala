package blunt

import doobie.imports._
import cats._, cats.implicits._
import shapeless._
import shapeless.ops.record._
import shapeless.ops.hlist._

import org.atteo.evo.inflector.English

import scala.reflect.runtime.universe.TypeTag

abstract class Queryable[T : Composite] {
  def columns: Seq[Fragment]
  def table: Fragment

  def allColumns = columns.toList.intercalate(fr",")
}

object Queryable {
  def apply[T : Composite](implicit q: Queryable[T]) = q

  def splitCamelCase(words: String): Seq[String] = {
    val indices = words.zipWithIndex.filter(_._1.isUpper).map(_._2)
    val (rest, result) = indices.foldRight((words, List.empty[String])) {
      case (index, (s, result)) =>
        val (rest, split) = s.splitAt(index)
        (rest, split :: result)
    }
    (rest :: result).filter(_.nonEmpty)
  }

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
