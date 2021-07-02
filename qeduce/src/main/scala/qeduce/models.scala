package qeduce

/**
 * Domain models as ADTs with corresponding database schemas.
 */ 
package models

import summit.{SumType, ProductType, SumOrProductType, Branch, BranchValue, Element, trace}
import java.sql.{PreparedStatement, Connection}

def insert[R](r: R)(using t: SQLTable[R])(using Connection): Int = t.insert(r)()
def declare[R](using t: SQLTable[R])(using Connection): Int = t.declare()()
def selectBranch[R](label: String)(using t: SQLModel[R]): QueryResult[R] = t.selectBranch(label)
def select[R](using t: SQLTable[R]): QueryResult[R] = t.select()
def insertAll[R](rs: Iterable[R])(using t: SQLTable[R])(using Connection): Int = t.insertAll(rs)()

trait SQLTable[R]:
  def insert(r: R): QueryResult[Int]
  def insertAll(rs: Iterable[R]): QueryResult[Int]
  def declare(): QueryResult[Int]
  def select(): QueryResult[R]

import Helpers._

object SQLTable:
  def derived[R](using prod: ProductType[R, SQLType]): SQLTable[R] =
    trace(s"deriving SQLTable[${prod.label}]")
    new SQLTable[R]:
      def insert(r: R): QueryResult[Int] = 
        insertTemplate(r, prod.label, prod.elements).update
      def declare(): QueryResult[Int] = 
        createTemplate(prod.label, prod.elements).update
      def select(): QueryResult[R] = 
        for r <- selectTemplate(prod.label, prod.elements).results
        yield prod.constructor(RowProduct(r, prod.elements))
      def insertAll(rs: Iterable[R]): QueryResult[Int] = 
        repeatedInsert(rs, prod.label, prod.elements)

trait SQLModel[R] extends SQLTable[R]:
  def selectBranch(label: String): QueryResult[R]

object SQLModel:
  def derived[R](using sum: SumType[R, SQLType]): SQLModel[R] =
    trace(s"deriving SQLModel[${sum.label}]")
    new SQLModel[R]:
      def insert(r: R): QueryResult[Int] = 
        val bv = sum.cast(r)
        insertTemplate(bv.value, bv.branch.label, bv.branch.elements).update

      def declare(): QueryResult[Int] = 
        val qrs = 
          for b <- sum.branches
          yield createTemplate(b.label, b.elements).update

        QueryResult.concat(qrs)

      def selectBranch(label: String): QueryResult[R] = 
        val b = sum.branches.find(_.label == label).get
        val q = selectTemplate(label, b.elements)
        q.results.map(r => b.upcast(b.constructor(RowProduct(r, b.elements))))

      def select(): QueryResult[R] =
        val qrs =
          for
            b <- sum.branches
            q = selectTemplate(b.label, b.elements)
          yield
            q.results.map(r => b.upcast(b.constructor(RowProduct(r, b.elements))))
        QueryResult.concat(qrs)

      def insertAll(rs: Iterable[R]): QueryResult[Int] = 
        val grps = rs.map(sum.cast(_)).groupBy(_.branch.ordinal)

        val qrs =
          for (_, bvs) <- grps
          yield
            val b = bvs.head.branch
            val vs = for bv <- bvs yield bv.value.asInstanceOf[b.B]
            repeatedInsert[b.B](vs, b.label, b.elements)

        QueryResult.concat(qrs)

object Helpers:
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

  def selectTemplate[B](label: String, elements: IndexedSeq[Element[B, SQLType]]): Query =
    sql"select" ~ colNames(elements) ~ sql"from" ~ Query(label)

  def repeatedInsert[B](bs: Iterable[B], label: String, elements: IndexedSeq[Element[B, SQLType]]): QueryResult[Int] =
    if bs.isEmpty then QueryResult.empty
    else
      val qy = insertTemplate(bs.head, label, elements)
      new QueryResult[Int]:
        def fold[S](s0: S)(f: (S, Int) => S)(using c: Connection): S =
          val n =
            qy.execute {
              (st) =>
                var n1 = st.executeUpdate()
                for b <- bs.tail
                do
                  for e <- elements
                  do e.typeclass.inject(st, e.index+1, e.pick(b))
                  n1 += st.executeUpdate()
                n1
            }
          f(s0, n)

class RowProduct[B](row: Row, elements: IndexedSeq[Element[B, SQLType]]) extends Product:
  def productElement(ix: Int): Any = 
    val e = elements(ix)
    row(e.label)(using e.typeclass)
  def productArity: Int = elements.size
  def canEqual(other: Any) = false

extension(rs: QueryResult[Row])
  def as[P](using t: ProductType[P, SQLType]): QueryResult[P] = 
    rs.map(r => t.constructor(RowProduct(r, t.elements)))
