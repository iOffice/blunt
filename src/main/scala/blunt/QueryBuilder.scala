package blunt

import scala.language.implicitConversions

import doobie.imports._
import cats._
import cats.implicits._
import shapeless._
import shapeless.poly._
import shapeless.ops.record._
import shapeless.syntax._
import shapeless.ops.hlist.{ Selector => _, _}
import shapeless.ops.tuple.{ Selector => TupleSelector }


sealed trait State
sealed trait Set extends State
sealed trait Unset extends State

class QueryBuilder[T : Composite, Select <: State, Update <: State, Insert <: State, 
                   Delete <: State, Where <: State, Join <: State] private 
                  (private val selectFrag: Option[Fragment] = None,
                   private val updateFrag: Seq[Fragment] = Seq.empty[Fragment],
                   private val insertFrag: Option[(Fragment, Seq[Fragment])] = None,
                   private val deleteFrag: Option[Fragment] = None,
                   private val whereFrag: Option[Fragment] = None,
                   private val joinFrag: Option[Fragment] = None)(
                   implicit private val queryable: Queryable[T]) {

  import QueryBuilder._

  def select(
    implicit ev1: Select =:= Unset,
    ev2: Update =:= Unset,
    ev3: Insert =:= Unset,
    ev4: Delete =:= Unset) = {
    val newSelect = fr"select " ++ queryable.allColumns ++ fr" from " ++ queryable.table
    new QueryBuilder[T, Set, Unset, Unset, Unset, Where, Join](
      Some(newSelect), updateFrag, insertFrag, deleteFrag, whereFrag, joinFrag)
  }

  def delete(
    implicit ev1: Select =:= Unset,
    ev2: Update =:= Unset,
    ev3: Insert =:= Unset,
    ev4: Delete =:= Unset) = {
    val newDelete = fr"delete from " ++ queryable.table
    new QueryBuilder[T, Unset, Unset, Unset, Set, Where, Join](
      selectFrag, updateFrag, insertFrag, Some(newDelete), whereFrag, joinFrag)
  }

  def update[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
    implicit generic: LabelledGeneric.Aux[T, H],
    selector: Selector.Aux[H, column.T, V],
    ev1: Select =:= Unset,
    ev2: Insert =:= Unset,
    ev3: Delete =:= Unset
  ) = {
    val newUpdate = updateFrag :+ eqFrag(column.value, value)
    new QueryBuilder[T, Unset, Set, Unset, Unset, Where, Join](
      selectFrag, newUpdate, insertFrag, deleteFrag, whereFrag, joinFrag)
  }

  def update[
    H <: HList, I, R <: HList, F <: HList, S, A](
    model: T, idColumn: Witness.Lt[Symbol])(
    implicit generic: LabelledGeneric.Aux[T, H],
    remover: Remover.Aux[H, idColumn.T, (I, R)],
    selector: Selector.Aux[H, idColumn.T, I],
    atomicId: Atom[I],
    fielder: Fields.Aux[R, (S, A) :: F],
    symbol: S <:< Symbol,
    atomic: Atom[A],
    folder: LeftFolder.Aux[F, Seq[Fragment], updateFolder.type, Seq[Fragment]],
    ev0: Select =:= Unset,
    ev1: Update =:= Unset,
    ev2: Insert =:= Unset,
    ev3: Delete =:= Unset,
    ev4: Where =:= Unset
  ) = {
    val (id, removed) = remover(generic.to(model))
    val fields = fielder(removed)
    val (headSymbol, headAtom) = fields.head
    val newUpdate = fields.tail.foldLeft(Seq(eqFrag(headSymbol, headAtom)))(updateFolder)
    new QueryBuilder[T, Unset, Set, Unset, Unset, Where, Unset](
      selectFrag, newUpdate, insertFrag, deleteFrag, whereFrag, joinFrag)
      .where(idColumn, id)
  }
  
  def insert[
    H <: HList, V, R <: HList, K <: HList, KH, KT <: HList, VS <: HList, VSH, VST <: HList](

    model: T, 
    idColumn: Witness.Lt[Symbol])(

    implicit generic: LabelledGeneric.Aux[T, H],
    remover: Remover.Aux[H, idColumn.T, (V, R)],
    tKeys: Keys.Aux[R, K],
    tValues: Values.Aux[R, VS],
    keysCons: IsHCons.Aux[K, KH, KT],
    symbolHead: KH <:< Symbol,
    symbolKeys: LUBConstraint[KT, Symbol],
    keysList: ToTraversable.Aux[KT, List, Symbol],
    valuesCons: IsHCons.Aux[VS, VSH, VST],
    valuesFolder: LeftFolder.Aux[VST, Fragment, insertValuesFolder.type, Fragment],
    atomic: Atom[VSH],
    ev1: Select =:= Unset,
    ev2: Update =:= Unset,
    ev3: Delete =:= Unset
  ) = {
    def makeValuesFragment: Fragment = {
      val (_, genRepr) = remover(generic.to(model))
      val hlistValues = tValues(genRepr)
      val head = hlistValues.head
      val tail = hlistValues.tail
      val valuesFrag = tail.foldLeft(fr"$head")(insertValuesFolder)
      fr"(" ++ valuesFrag ++ fr")"
    }

    val newInsert = insertFrag.map({ case (insertInto, values) =>
      (insertInto, values :+ makeValuesFragment)
    }).orElse(Option({
      val head = tKeys.apply.head
      val tail = tKeys.apply.tail
      val columnsFrag = tail.toList.foldLeft(Fragment.const(head.name))({
        (acc, symbol) => acc ++ fr"," ++ Fragment.const(symbol.name)
      })
      (fr"(" ++ columnsFrag ++ fr")", Seq(makeValuesFragment))
    }))
    new QueryBuilder[T, Unset, Unset, Set, Unset, Where, Join](
      selectFrag, updateFrag, newInsert, deleteFrag, whereFrag, joinFrag)
  }

  def join[S : Composite](
    implicit q: Queryable[S],
    ev0: Select =:= Set,
    ev1: Join =:= Unset,
    ev2: Update =:= Unset,
    ev3: Delete =:= Unset
  ) = new JoinExtension[T, S, Select, Update, Insert, Delete, Where] {
    def on[
      H <: HList, L <: HList, U, V](

      leftColumn: Witness.Lt[Symbol], 
      rightColumn: Witness.Lt[Symbol])(

      implicit leftGen: LabelledGeneric.Aux[T, H],
      rightGen: LabelledGeneric.Aux[S, L],
      leftSel: Selector.Aux[H, leftColumn.T, U],
      rightSel: Selector.Aux[L, rightColumn.T, V],
      atom1: Atom[U],
      atom2: Atom[V],
      ev: U =:= V
    ) = {
      val newSelect = fr"select" ++ 
        queryable.columns.toList.map(dot(queryable.table, _)).intercalate(fr",") ++
        fr"," ++ q.columns.toList.map(dot(q.table, _)).intercalate(fr",") ++
        fr" from " ++ queryable.table
      val newJoin = fr" join " ++ q.table ++ 
        fr" on " ++ dot(queryable.table, Fragment.const(leftColumn.value.name)) ++ 
        fr0" = " ++ dot(q.table, Fragment.const(rightColumn.value.name))
      new QueryBuilder[(T, S), Select, Update, Insert, Delete, Where, Set](
        Some(newSelect), updateFrag, insertFrag, deleteFrag, whereFrag, Some(newJoin))
    }
  }

  def where[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
    implicit generic: LabelledGeneric.Aux[T, H],
    selector: Selector.Aux[H, column.T, V],
    ev0: Where =:= Unset,
    ev1: Join =:= Unset
  ) = {
    val newWhere = fr" where " ++ eqFrag(column.value, value)
    new QueryBuilder[T, Select, Update, Insert, Delete, Set, Join](
      selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
  }

  def and[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[T, H],
      selector: Selector.Aux[H, column.T, V],
      ev: Where =:= Set) = {
    val newWhere = whereFrag
      .map(_ ++ fr" and " ++ eqFrag(column.value, value))
      .getOrElse(fr" where " ++ eqFrag(column.value, value))
    new QueryBuilder[T, Select, Update, Insert, Delete, Set, Join](
      selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
  }

  def or[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[T, H],
      selector: Selector.Aux[H, column.T, V],
      ev: Where =:= Set) = {
    val newWhere = whereFrag
      .map(_ ++ fr" or " ++ eqFrag(column.value, value))
      .getOrElse(fr" where " ++ eqFrag(column.value, value))
    new QueryBuilder[T, Select, Update, Insert, Delete, Set, Join](
      selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
  }

  def project[S : Composite](
    implicit tuple: T <:< (_, _),
    sel1: TupleSelector.Aux[T, S],
    q: Queryable[S],
    ev0: Join =:= Set
  ) = new {
    def where[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[S, H],
      selector: Selector.Aux[H, column.T, V],
      ev1: Where =:= Unset
    ) = {
      val newWhere = fr" where " ++ scopedEq(q.table, column.value, value)
      new QueryBuilder[T, Set, Unset, Unset, Unset, Set, Set](
         selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
    }

    def and[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[S, H],
      selector: Selector.Aux[H, column.T, V],
      ev1: Where =:= Set
    ) = {
      val newWhere = whereFrag
        .map(_ ++ fr" and " ++ scopedEq(q.table, column.value, value))
        .getOrElse(fr" where " ++ scopedEq(q.table, column.value, value))
      new QueryBuilder[T, Set, Unset, Unset, Unset, Set, Set](
        selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
    }

    def or[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[S, H],
      selector: Selector.Aux[H, column.T, V],
      ev1: Where =:= Unset
    ) = {
      val newWhere = whereFrag
        .map(_ ++ fr" or " ++ scopedEq(q.table, column.value, value))
        .getOrElse(fr" where " ++ scopedEq(q.table, column.value, value))
      new QueryBuilder[T, Set, Unset, Unset, Unset, Set, Set](
         selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
    }
  }
}

object QueryBuilder {
  def apply[T : Composite : Queryable] = 
    new QueryBuilder[T, Unset, Unset, Unset, Unset, Unset, Unset]

  def dot(table: Fragment, column: Fragment) =
    table ++ fr0"." ++ column

  def eqFrag[V : Atom](column: Symbol, value: V): Fragment = 
    eqFrag(Fragment.const(column.name), fr"$value")

  def eqFrag(column: Fragment, value: Fragment): Fragment = 
    column ++ fr"=" ++ value

  def scopedEq[V : Atom](table: Fragment, column: Symbol, value: V): Fragment =
    scopedEq(table, Fragment.const(column.name), fr"$value")

  def scopedEq(table: Fragment, column: Fragment, value: Fragment) =
    eqFrag(dot(table, column), value)

  object updateFolder extends Poly2 {
    implicit def eq[T : Atom, S <: Symbol]: Case.Aux[Seq[Fragment], (S, T), Seq[Fragment]] = 
      at({ 
        case (acc, (symbol, atom)) => 
          acc :+ Fragment.const(symbol.name) ++ fr" = " ++ fr"$atom" 
      })
  }

  object insertValuesFolder extends Poly2 {
    implicit def atom[T : Atom]: Case.Aux[Fragment, T, Fragment] = 
      at((acc, t) => acc ++ fr"," ++ fr"$t")
  }

  // This banishes the reflection warning
  abstract class JoinExtension[T : Composite : Queryable, S : Composite : Queryable, 
    Select <: State, Update <: State, Insert <: State, Delete <: State, Where <: State] {
    def on[H <: HList, L <: HList, U, V](
    leftColumn: Witness.Lt[Symbol], rightColumn: Witness.Lt[Symbol])( 
    implicit leftGen: LabelledGeneric.Aux[T, H],
    rightGen: LabelledGeneric.Aux[S, L],
    leftSel: Selector.Aux[H, leftColumn.T, U],
    rightSel: Selector.Aux[L, rightColumn.T, V],
    atom1: Atom[U],
    atom2: Atom[V],
    ev: U =:= V): QueryBuilder[(T, S), Select, Update, Insert, Delete, Where, Set]
  }

  trait BuildQuery {
    // implicit val han = LogHandler.jdkLogHandler
  }

  implicit class BuildSelect[T : Composite : Queryable](
    qb: QueryBuilder[T, Set, Unset, Unset, Unset, _, _]
  )(implicit log: LogHandler = LogHandler.nop) extends BuildQuery {
    def build: Query0[T] =
      (qb.selectFrag.getOrElse(fr"") ++ 
       qb.joinFrag.getOrElse(fr"") ++ 
       qb.whereFrag.getOrElse(fr""))
         .query[T]
  }

  implicit class BuildDelete[T : Composite : Queryable](
    qb: QueryBuilder[T, Unset, Unset, Unset, Set, _, _]
  )(implicit log: LogHandler = LogHandler.nop) extends BuildQuery {
    def build: Update0 =
      (qb.deleteFrag.getOrElse(fr"") ++ 
       qb.joinFrag.getOrElse(fr"") ++ 
       qb.whereFrag.getOrElse(fr""))
         .update
  }

  implicit class BuildUpdate[T : Composite : Queryable](
    qb: QueryBuilder[T, Unset, Set, Unset, Unset, _, _]
  )(implicit log: LogHandler = LogHandler.nop) extends BuildQuery {
    def build: Update0 = {
      val updateFrag = fr"update " ++ qb.queryable.table ++ fr" set " ++
        qb.updateFrag.toList.intercalate(fr",")
      (updateFrag ++
       qb.joinFrag.getOrElse(fr"") ++ 
       qb.whereFrag.getOrElse(fr""))
         .update
    }
  }

  implicit class BuildInsert[T : Composite : Queryable](
    qb: QueryBuilder[T, Unset, Unset, Set, Unset, _, _]
  )(implicit log: LogHandler = LogHandler.nop) extends BuildQuery {
    def build: Update0 = qb.insertFrag
      .flatMap({ case (columnsFrag, values) => 
        values 
          .drop(1)
          .foldLeft(values.headOption)((a, b) => a.map(_ ++ fr"," ++ b))
          .map(fr"insert into" ++ qb.queryable.table  
            ++ columnsFrag ++ fr"values" ++ _)
      })
      .getOrElse(fr"")
      .update
  }
}
