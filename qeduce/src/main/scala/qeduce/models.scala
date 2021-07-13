package qeduce

/**
 * Domain models as ADTs with corresponding database schemas.
 */ 
package models

import summit.{SumType, ProductType, SumOrProductType, Branch, BranchValue, Element, trace}
import java.sql.{PreparedStatement, Connection}
import geny.Generator
import qeduce.helpers._

export qeduce.helpers.{SelectTemplate, allRows}

def declare[R](using t: SQLTable[R]): Connection ?=> Int = t.declare()
def insert[R](r: R)(using t: SQLTable[R]): Connection ?=> Int = t.insert(r)
def insertAll[R](rs: Iterable[R])(using t: SQLTable[R]): Connection ?=> Int = t.insertAll(rs)
def select[R](using t: SQLTable[R]): Connection ?=> Generator[R] = t.select(allRows)
def select[R](template: SelectTemplate)(using t: SQLTable[R]): Connection ?=> Generator[R] = t.select(template)
def selectBranch[R](label: String, template: SelectTemplate = allRows)(using t: SQLModel[R]): Connection ?=> Generator[R] = 
  t.selectBranch(label, template)

trait SQLTable[R]:
  def declare(): Connection ?=> Int
  def insert(r: R): Connection ?=> Int
  def insertAll(rs: Iterable[R]): Connection ?=> Int
  def select(template: SelectTemplate): Connection ?=> Generator[R]

object SQLTable:
  def derived[R](using prod: ProductType[R, SQLType]): SQLTable[R] =
    trace(s"deriving SQLTable[${prod.label}]")
    new SQLTable[R]:
      def insert(r: R): Connection ?=> Int = 
        insertTemplate(r, prod.label, prod.elements).execute()
      def declare(): Connection ?=> Int = 
        createTemplate(prod.label, prod.elements).execute()
      def select(template: SelectTemplate): Connection ?=> Generator[R] = 
        for r <- expandSelect(prod.label, prod.elements, template).results
        yield prod.constructor(RowProduct(r, prod.elements))
      def insertAll(rs: Iterable[R]): Connection ?=> Int = 
        repeatedInsert(rs, prod.label, prod.elements)

trait SQLModel[R] extends SQLTable[R]:
  def selectBranch(label: String, template: SelectTemplate): Connection ?=> Generator[R]

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

      def selectBranch(label: String, template: SelectTemplate): Connection ?=> Generator[R] = 
        val b = sum.branches.find(_.label == label).get
        val q = expandSelect(label, b.elements, template)
        q.results.map(r => b.upcast(b.constructor(RowProduct(r, b.elements))))

      def select(template: SelectTemplate): Connection ?=> Generator[R] =
        for
          b <- Generator.from(sum.branches)
          q = expandSelect(b.label, b.elements, template)
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

extension(rs: Connection ?=> Generator[Row])
  def as[P](using t: ProductType[P, SQLType]): Connection ?=> Generator[P] = 
    rs.map(r => t.constructor(RowProduct(r, t.elements)))
