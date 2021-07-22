package qeduce

/**
 * Domain models as ADTs with corresponding database schemas.
 */ 
package models

import summit.{SumType, ProductType}
import java.sql.Connection
import geny.Generator
import qeduce.helpers._

export qeduce.helpers.{SelectTemplate, allRows}

type SQLTable[R] = ProductType[R, SQLType]
type SQLModel[R] = SumType[R, SQLType]

def declare[R](using prod: SQLTable[R]): Connection ?=> Int = 
  createTemplate(prod.label, prod.elements).execute()

def insert[R](r: R)(using prod: SQLTable[R]): Connection ?=> Int = 
  insertTemplate(r, prod.label, prod.elements).execute()

def insertAll[R](rs: Iterable[R])(using prod: SQLTable[R]): Connection ?=> Int = 
  repeatedInsert(rs, prod.label, prod.elements)

def select[R](using prod: SQLTable[R]): Connection ?=> Generator[R] = select(allRows)(using prod)

def select[R](template: SelectTemplate)(using prod: SQLTable[R]): Connection ?=> Generator[R] =
  for r <- expandSelect(prod.label, prod.elements, template).results
  yield prod.constructor(RowProduct(r, prod.elements))

def declare[R](using sum: SQLModel[R]): Connection ?=> Int = 
  var n = 0
  for b <- sum.branches
  do n += createTemplate(b.label, b.elements).execute()
  n

def insert[R](r: R)(using sum: SQLModel[R]): Connection ?=> Int = 
  val bv = sum.cast(r)
  insertTemplate(bv.value, bv.branch.label, bv.branch.elements).execute()
  
def insertAll[R](rs: Iterable[R])(using sum: SQLModel[R]): Connection ?=> Int = 
  var n = 0
  val grps = rs.map(sum.cast(_)).groupBy(_.branch.ordinal)
  for (_, bvs) <- grps
  do
    val b = bvs.head.branch
    val vs = for bv <- bvs yield bv.value.asInstanceOf[b.B]
    n += repeatedInsert[b.B](vs, b.label, b.elements)
  n

def select[R](using sum: SQLModel[R]): Connection ?=> Generator[R] = select(allRows)(using sum)

def select[R](template: SelectTemplate)(using sum: SQLModel[R]): Connection ?=> Generator[R] = 
  for
    b <- Generator.from(sum.branches)
    q = expandSelect(b.label, b.elements, template)
    r <- q.results
  yield
    b.upcast(b.constructor(RowProduct(r, b.elements)))

def selectBranch[R](label: String, template: SelectTemplate = allRows)(using sum: SQLModel[R]): Connection ?=> Generator[R] = 
  val b = sum.branches.find(_.label == label).get
  val q = expandSelect(label, b.elements, template)
  q.results.map(r => b.upcast(b.constructor(RowProduct(r, b.elements))))

extension(rs: Connection ?=> Generator[Row])
  def as[P](using t: ProductType[P, SQLType]): Connection ?=> Generator[P] = 
    rs.map(r => t.constructor(RowProduct(r, t.elements)))
