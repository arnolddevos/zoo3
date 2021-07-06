package qeduce

/**
 * Domain models as ADTs with corresponding database schemas.
 */ 
package models

import summit.{SumType, ProductType, SumOrProductType, Branch, BranchValue, Element, trace}
import java.sql.{PreparedStatement, Connection}
import geny.Generator
import helpers._

def declare[R](using t: SQLCore[R]): Connection ?=> Int = t.declare()
def insert[R](r: R)(using t: SQLCore[R]): Connection ?=> Int = t.insert(r)
def insertAll[R](rs: Iterable[R])(using t: SQLCore[R]): Connection ?=> Int = t.insertAll(rs)
def select[R](using t: SQLCore[R]): Connection ?=> Generator[R] = t.select(Query.empty)
def select[R](clause: Query)(using t: SQLCore[R]): Connection ?=> Generator[R] = t.select(clause)
def selectBranch[R](label: String, clause: Query = Query.empty)(using t: SQLModel[R]): Connection ?=> Generator[R] = 
  t.selectBranch(label, clause)

trait SQLCore[R]:
  def declare(): Connection ?=> Int
  def insert(r: R): Connection ?=> Int
  def insertAll(rs: Iterable[R]): Connection ?=> Int
  def select(clause: Query): Connection ?=> Generator[R]

trait SQLTable[R] extends SQLCore[R]:
  def columns: Query
  def values(r: R): Query

object SQLTable:
  def derived[R](using prod: ProductType[R, SQLType]): SQLTable[R] =
    trace(s"deriving SQLTable[${prod.label}]")
    new SQLTable[R]:
      def insert(r: R): Connection ?=> Int = 
        insertTemplate(r, prod.label, prod.elements).execute()
      def declare(): Connection ?=> Int = 
        createTemplate(prod.label, prod.elements).execute()
      def select(clause: Query): Connection ?=> Generator[R] = 
        for r <- selectTemplate(prod.label, prod.elements, clause).results
        yield prod.constructor(RowProduct(r, prod.elements))
      def insertAll(rs: Iterable[R]): Connection ?=> Int = 
        repeatedInsert(rs, prod.label, prod.elements)
      def columns: Query = colNames(prod.elements)
      def values(r: R): Query = colValues(r, prod.elements)

trait SQLModel[R] extends SQLCore[R]:
  def selectBranch(label: String, clause: Query): Connection ?=> Generator[R]

object SQLModel:
  def derived[R](using sum: SumType[R, SQLType]): SQLModel[R] =
    trace(s"deriving SQLModel[${sum.label}]")
    new SQLModel[R]:
      def insert(r: R): Connection ?=> Int = 
        val bv = sum.cast(r)
        insertTemplate(bv.value, bv.branch.label, bv.branch.elements).execute()

      def declare(): Connection ?=> Int = 
        var n = 0
        for b <- sum.branches
        do n += createTemplate(b.label, b.elements).execute()
        n

      def selectBranch(label: String, clause: Query): Connection ?=> Generator[R] = 
        val b = sum.branches.find(_.label == label).get
        val q = selectTemplate(label, b.elements, clause)
        q.results.map(r => b.upcast(b.constructor(RowProduct(r, b.elements))))

      def select(clause: Query): Connection ?=> Generator[R] =
        for
          b <- Generator.from(sum.branches)
          q = selectTemplate(b.label, b.elements, clause)
          r <- q.results
        yield
          b.upcast(b.constructor(RowProduct(r, b.elements)))

      def insertAll(rs: Iterable[R]): Connection ?=> Int = 
        var n = 0
        val grps = rs.map(sum.cast(_)).groupBy(_.branch.ordinal)
        for (_, bvs) <- grps
        do
          val b = bvs.head.branch
          val vs = for bv <- bvs yield bv.value.asInstanceOf[b.B]
          n += repeatedInsert[b.B](vs, b.label, b.elements)
        n

package helpers:
  def colNames[B](elements: IndexedSeq[Element[B, SQLType]]): Query =
    val ls =
      for e <- elements
      yield e.label
    Query(ls.mkString(", "))

  def colValues[B](b: B, elements: IndexedSeq[Element[B, SQLType]]): Query =
    val ps =
      for e <- elements
      yield Param(e.pick(b))(using e.typeclass)
    Query(ps)

  def insertTemplate[B](b: B, label: String, elements: IndexedSeq[Element[B, SQLType]]): Query =
    sql"insert into" ~ Query(label) ~ 
    sql"("  ~ colNames(elements) ~ 
    sql") values (" ~ colValues(b, elements) ~ sql")"

  def createTemplate[B](label: String, elements: IndexedSeq[Element[B, SQLType]]): Query =
    sql"create table if not exists" ~ Query(label) ~ 
    sql"(" ~ colNames(elements) ~ sql")"

  def selectTemplate[B](label: String, elements: IndexedSeq[Element[B, SQLType]], clause: Query): Query =
    sql"select" ~ colNames(elements) ~ sql"from" ~ Query(label) ~ clause

  def repeatedInsert[B](bs: Iterable[B], label: String, elements: IndexedSeq[Element[B, SQLType]]): Connection ?=> Int =
    var n = 0
    if ! bs.isEmpty then
      val qy = insertTemplate(bs.head, label, elements)
      for st <- qy.prepare()
      do 
        n += st.executeUpdate()
        for b <- bs.tail
        do
          for e <- elements
          do e.typeclass.inject(st, e.index+1, e.pick(b))
          n += st.executeUpdate()
    n

  class RowProduct[B](row: Row, elements: IndexedSeq[Element[B, SQLType]]) extends Product:
    def productElement(ix: Int): Any = 
      val e = elements(ix)
      row(e.label)(using e.typeclass)
    def productArity: Int = elements.size
    def canEqual(other: Any) = false

extension(rs: Connection ?=> Generator[Row])
  def as[P](using t: ProductType[P, SQLType]): Connection ?=> Generator[P] = 
    rs.map(r => t.constructor(RowProduct(r, t.elements)))
