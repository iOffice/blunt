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

import scala.reflect.runtime.universe.TypeTag


/*
 * The State ADT is used to keep track of query builder state in the type level.
 * It's essentially a typelevel state machine, where the methods on `QueryBuilder`
 * are transistions.
 */
sealed trait State
sealed trait Set extends State
sealed trait Unset extends State

/*
 * A QueryBuilder can be made for some `Composite` `T`, and keeps track at
 * the type level whether the query has be set to be a Select, Update, 
 * Insert, Delete, as well as whether a Where or Join clause has been added.
 * A `Queryable` instance for `T`  is require to create a query builder for `T`
 */
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

  /*
   * `select` ensures that no other query clause has been set, then returns
   * a new `QueryBuilder` with the select clause set, querying for all columns.
   * It's compatible with join and where.
   */
  def select(
    implicit ev1: Select =:= Unset,
    ev2: Update =:= Unset,
    ev3: Insert =:= Unset,
    ev4: Delete =:= Unset) = {
    val newSelect = fr"select " ++ queryable.allColumns ++ fr" from " ++ queryable.table
    new QueryBuilder[T, Set, Unset, Unset, Unset, Where, Join](
      Some(newSelect), updateFrag, insertFrag, deleteFrag, whereFrag, joinFrag)
  }

  /* 
   * `delete` ensures that no other query clause has been set, and returns a new
   * `QueryBuilder` with the delete clause set. It's then be used with a call
   * to `where`.
   */
  def delete(
    implicit ev1: Select =:= Unset,
    ev2: Update =:= Unset,
    ev3: Insert =:= Unset,
    ev4: Delete =:= Unset) = {
    val newDelete = fr"delete from " ++ queryable.table
    new QueryBuilder[T, Unset, Unset, Unset, Set, Where, Join](
      selectFrag, updateFrag, insertFrag, Some(newDelete), whereFrag, joinFrag)
  }

  /* 
   * This `update` method takes a single column and updates it to a given value.
   * Like all the others it makes sure none of the other clauses have been set yet.
   * However, unlike the other methods, this one can be called multiple times allowing
   * for several updates to be applied in one query.
   */
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

  /*
   * This `update` take a `T` and a symbol for the id column and updates
   * that table to the values of the fields of `T`. It requires a bunch of
   * implicits in order to ensure everything checks out, which probably 
   * increases compile time quite a bit. It might be worth looking into
   * removing/caching some of these somewhow.
   */
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
  
  /*
   * `insert` is similar to `update`, it takes a `T` and an id column symbol and does
   * some implicit checks to make sure those are valid, as well as some other implicit
   * params for converting `T` into a list of keys and values so it can be converted 
   * to SQL. `insert` can be called multiple times to allow for inserting multiple `T`'s
   * in one SQL call (bulk insert).
   */
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

    // The insert fragment is represented as tuple of a fragment that is the
    // `"INSERT INTO <TABLE_NAME>" (column1, column2, ...)` part of the SQL 
    // statement, as well as a list of values sets like `"(value1, value2, ...)"`
    // to allow for multiple calls to insert in one query (i.e. bulk insert)
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

  /*
   * The following "SomethingApply" pattern is used to allow for the API 
   * that we want i.e. For `join` we want it to look like this:
   * `QueryBuilder[Table].join[OtherTable].on[=]('field1, 'field2)`
   * so `join` needs to return a another object, and just using an anonymous
   * one gives a bunch of warnings, so this is the work around.
   */
  class OnApply[S : Composite, O <: Operator](implicit sQueryable: Queryable[S]) {
    /*
     * `OnApply`'s `apply` method has to do a lot of implicit stuff. It 
     * needs to verify both the columns provided belong to their respective
     * entities `T` and `S`, as well as get the SQL value for the operator
     * provided as a type parameter. Because it returns a `QueryBuilder[(T, S)]`
     * it has to make sure that `(T, S)` is a Composite.
     */
    def apply[H <: HList, L <: HList, U, V](
      leftColumn: Witness.Lt[Symbol], rightColumn: Witness.Lt[Symbol])( 
      implicit leftGen: LabelledGeneric.Aux[T, H],
      tupComp: Composite[(T, S)],
      typeTag: TypeTag[(T, S)],
      rightGen: LabelledGeneric.Aux[S, L],
      leftSel: Selector.Aux[H, leftColumn.T, U],
      rightSel: Selector.Aux[L, rightColumn.T, V],
      atom1: Atom[U],
      atom2: Atom[V],
      defaultOp: DefaultsTo[O, eql],
      opSql: ToSql[O],
      ev: U =:= V
    ): QueryBuilder[(T, S), Select, Update, Insert, Delete, Where, Set] = {
      val newSelect = fr"select" ++ 
        queryable.columns.toList.map(dot(queryable.table, _)).intercalate(fr",") ++
        fr"," ++ sQueryable.columns.toList.map(dot(sQueryable.table, _)).intercalate(fr",") ++
        fr" from " ++ queryable.table
      val newJoin = fr" join " ++ sQueryable.table ++ 
        fr" on " ++ dot(queryable.table, Fragment.const(leftColumn.value.name)) ++ 
        opSql.sql ++ dot(sQueryable.table, Fragment.const(rightColumn.value.name))
      new QueryBuilder[(T, S), Select, Update, Insert, Delete, Where, Set](
        Some(newSelect), updateFrag, insertFrag, deleteFrag, whereFrag, Some(newJoin))
    }
  }

  class JoinExtension[S : Composite : Queryable] {
    def on[O <: Operator] = new OnApply[S, O]
  }

  def join[S : Composite](
    implicit q: Queryable[S],
    ev0: Select =:= Set,
    ev1: Join =:= Unset,
    ev2: Update =:= Unset,
    ev3: Delete =:= Unset
  ) = new JoinExtension[S]

  /*
   * `WhereApply` is another example of the apply pattern. Again, we need
   * to do this to accept operators as a type parameter and not get annoying
   * warnings from usin anonymous objects. Example usage: 
   * `QueryBuilder[Table].select.where[<]('population, 1000)`
   */
  abstract class WhereApply[O <: Operator, S : Composite : Queryable] {
    /*
     * `apply` checks that the given column is a field of the given entity,
     * and that where hasn't been called before on this query builder. It
     * also does some implicit trickery to make the operator default to `eql`
     */
    def apply[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[S, H],
      selector: Selector.Aux[H, column.T, V],
      ev0: Where =:= Unset,
      defaultOp: DefaultsTo[O, eql],
      opSql: ToSql[O]
    ): QueryBuilder[T, _, _, _, _, _, _]
  }

  /*
   * `AndOrApply` is an abstract class for all of the examples where we need
   * an `and` or `or` (their type signatures are the same). Both `and` and `or`
   * are similar
   */
  abstract class AndOrApply[O <: Operator, S : Composite : Queryable] {
    def apply[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[S, H],
      selector: Selector.Aux[H, column.T, V],
      ev0: Where =:= Set, // Both and and or only work with where set
      defaultOp: DefaultsTo[O, eql],
      opSql: ToSql[O]
    ): QueryBuilder[T, _, _, _, _, _, _]
  }

  /*
   * Adds a where clause to a query, given a column, value, and typelevel
   * operator.
   */
  def where[O <: Operator] = new WhereApply[O, T] { 
    def apply[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[T, H],
      selector: Selector.Aux[H, column.T, V],
      ev0: Where =:= Unset,
      defaultOp: DefaultsTo[O, eql],
      opSql: ToSql[O]
    ) = {
      val newWhere = fr" where " ++ opFrag(column.value, opSql.sql, value)
      new QueryBuilder[T, Select, Update, Insert, Delete, Set, Join](
        selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
    }
  }

  /*
   * Adds an and clause to a query assuming a where clause has already been set.
   * This can be called multiple times to have multiple and statements.
   */
  def and[O <: Operator] = new AndOrApply[O, T] {
    def apply[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[T, H],
      selector: Selector.Aux[H, column.T, V],
      ev: Where =:= Set,
      defaultOp: DefaultsTo[O, eql],
      opSql: ToSql[O]) = {
        val newWhere = whereFrag
          .map(_ ++ fr" and " ++ opFrag(column.value, opSql.sql, value))
          .getOrElse(fr" where " ++ opFrag(column.value, opSql.sql, value))
        new QueryBuilder[T, Select, Update, Insert, Delete, Set, Join](
          selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
    }
  }

  /*
   * Adds an or clause to a query assuming a where clause has already been set.
   * This can be called multiple times to have multiple or statements.
   */
  def or[O <: Operator] = new AndOrApply[O, T] {
    def apply[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
      implicit generic: LabelledGeneric.Aux[T, H],
      selector: Selector.Aux[H, column.T, V],
      ev: Where =:= Set,
      defaultOp: DefaultsTo[O, eql],
      opSql: ToSql[O]) = {
        val newWhere = whereFrag
          .map(_ ++ fr" or " ++ opFrag(column.value, opSql.sql, value))
          .getOrElse(fr" where " ++ opFrag(column.value, opSql.sql, value))
        new QueryBuilder[T, Select, Update, Insert, Delete, Set, Join](
          selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
    }
  }

  /*
   * The projection extentions are for when the user called project on a query builder
   * for a tuple of two entities, like `(T, S)`.  All the query methods work the same
   * as they do on the top level query builder, except they operate on the selected
   * entity.
   */
  class ProjectExtension[S : Composite](implicit sQueryable: Queryable[S]) {
    def where[O <: Operator] = new WhereApply[O, S] {
      def apply[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
        implicit generic: LabelledGeneric.Aux[S, H],
        selector: Selector.Aux[H, column.T, V],
        ev1: Where =:= Unset,
        defaultOp: DefaultsTo[O, eql],
        opSql: ToSql[O]
      ) = {
        val newWhere = fr" where " ++ scopedOp(sQueryable.table, column.value, opSql.sql, value)
        new QueryBuilder[T, Set, Unset, Unset, Unset, Set, Set](
          selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
      }
    }

    def and[O <: Operator] = new AndOrApply[O, S] {
      def apply[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
        implicit generic: LabelledGeneric.Aux[S, H],
        selector: Selector.Aux[H, column.T, V],
        ev1: Where =:= Set,
        defaultOp: DefaultsTo[O, eql],
        opSql: ToSql[O]
      ) = {
        val newWhere = whereFrag
          .map(_ ++ fr" and " ++ scopedOp(sQueryable.table, column.value, opSql.sql, value))
          .getOrElse(fr" where " ++ scopedOp(sQueryable.table, column.value, opSql.sql, value))
        new QueryBuilder[T, Set, Unset, Unset, Unset, Set, Set](
          selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
      }
    }

    def or[O <: Operator] = new AndOrApply[O, S] {
      def apply[V : Atom, H <: HList](column: Witness.Lt[Symbol], value: V)(
        implicit generic: LabelledGeneric.Aux[S, H],
        selector: Selector.Aux[H, column.T, V],
        ev1: Where =:= Set,
        defaultOp: DefaultsTo[O, eql],
        opSql: ToSql[O]
      ) = {
        val newWhere = whereFrag
          .map(_ ++ fr" or " ++ scopedOp(sQueryable.table, column.value, opSql.sql, value))
          .getOrElse(fr" where " ++ scopedOp(sQueryable.table, column.value, opSql.sql, value))
        new QueryBuilder[T, Set, Unset, Unset, Unset, Set, Set](
           selectFrag, updateFrag, insertFrag, deleteFrag, Some(newWhere), joinFrag)
      }
    }
  }

  /*
   * Project is the way the user applies query statements to different tables in
   * the case of a join. The provided `S` type param is the entity for the table
   * you wish to apply the query to. There is some type level implict logic to
   * make sure that `S` is a member of the tuple for the current query builder.
   */
  def project[S : Composite](
    implicit tuple: T <:< (_, _),
    sel1: TupleSelector.Aux[T, S],
    q: Queryable[S],
    ev0: Join =:= Set
  ) = new ProjectExtension[S]
}

object QueryBuilder {
  /*
   * This is the main way to construct a `QueryBuilder`. Gives a `QueryBuilder`
   * for the given `T` with all other type params set to `Unset`.
   */
  def apply[T : Composite : Queryable] = 
    new QueryBuilder[T, Unset, Unset, Unset, Unset, Unset, Unset]

  /*
   * Below are some helper function for constructing fragment
   */
  def dot(table: Fragment, column: Fragment) =
    table ++ fr0"." ++ column

  def eqFrag[V : Atom](column: Symbol, value: V): Fragment = 
    eqFrag(Fragment.const(column.name), fr"$value")

  def eqFrag(column: Fragment, value: Fragment): Fragment = 
    column ++ fr"=" ++ value

  def opFrag(column: Fragment, op: Fragment, value: Fragment): Fragment = 
    column ++ op ++ value

  def opFrag[V : Atom](column: Symbol, op: Fragment, value: V): Fragment =
    opFrag(Fragment.const(column.name), op, fr"$value")

  def scopedEq[V : Atom](table: Fragment, column: Symbol, value: V): Fragment =
    scopedEq(table, Fragment.const(column.name), fr"$value")

  def scopedEq(table: Fragment, column: Fragment, value: Fragment) =
    eqFrag(dot(table, column), value)

  def scopedOp[V : Atom](table: Fragment, column: Symbol, op: Fragment, value: V): Fragment =
    opFrag(dot(table, Fragment.const(column.name)), op, fr"$value")

  /*
   * This is a shapeless polymorphic function for folding an HList of values
   * for use in the `update` method
   */
  object updateFolder extends Poly2 {
    implicit def eq[T : Atom, S <: Symbol]: Case.Aux[Seq[Fragment], (S, T), Seq[Fragment]] = 
      at({ 
        case (acc, (symbol, atom)) => 
          acc :+ Fragment.const(symbol.name) ++ fr" = " ++ fr"$atom" 
      })
  }

  /*
   * This is a shapeless polymorphic function similar to `updateFolder`
   * but joins the fragements for insert instead of the update
   */
  object insertValuesFolder extends Poly2 {
    implicit def atom[T : Atom]: Case.Aux[Fragment, T, Fragment] = 
      at((acc, t) => acc ++ fr"," ++ fr"$t")
  }

  /*
   * Below are implicit classes to provide a uniform `build`
   * function that takes the appropriate `QueryBuilder` and
   * returns the appropriate doobie query/update type.
   */
  implicit class BuildSelect[T : Composite : Queryable](
    qb: QueryBuilder[T, Set, Unset, Unset, Unset, _, _]
  )(implicit log: LogHandler = LogHandler.nop) {
    def build: Query0[T] =
      (qb.selectFrag.getOrElse(fr"") ++ 
       qb.joinFrag.getOrElse(fr"") ++ 
       qb.whereFrag.getOrElse(fr""))
         .query[T]
  }

  implicit class BuildDelete[T : Composite : Queryable](
    qb: QueryBuilder[T, Unset, Unset, Unset, Set, _, _]
  )(implicit log: LogHandler = LogHandler.nop) {
    def build: Update0 =
      (qb.deleteFrag.getOrElse(fr"") ++ 
       qb.joinFrag.getOrElse(fr"") ++ 
       qb.whereFrag.getOrElse(fr""))
         .update
  }

  implicit class BuildUpdate[T : Composite : Queryable](
    qb: QueryBuilder[T, Unset, Set, Unset, Unset, _, _]
  )(implicit log: LogHandler = LogHandler.nop) {
    def build: Update0 = {
      // Unlike the other query types, update isn't fully built yet
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
  )(implicit log: LogHandler = LogHandler.nop) {
    // We do some extra logic at the build step for insert
    // in order to allow for inserting of multiple values
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
