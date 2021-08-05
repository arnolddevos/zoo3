package qeduce

/**
 * Domain models as ADTs with corresponding database schemas.
 */ 
package models

import summit.{SumType, ProductType, Branch, BranchValue, Element}
import java.sql.Connection
import geny.Generator
import helpers._

export helpers.{SelectTemplate, selectTemplate}

case class SQLTable[R](prod: ProductType[R, SQLType])

object SQLTable:
  def derived[R](using prod: ProductType[R, SQLType]): SQLTable[R] = SQLTable(prod)

case class SQLModel[R](sum: SumType[R, SQLType])

object SQLModel:
  def derived[R](using sum: SumType[R, SQLType]): SQLModel[R] = SQLModel(sum)

def tableName[R](using t: SQLTable[R]) = Query(t.prod.label)

def tableName[R](b: Branch[R, SQLType]) = Query(b.label)

def attribNames[R](using t: SQLTable[R]) = colNames(t.prod.elements)

def attribNames[R](b: Branch[R, SQLType]) = colNames(b.elements)

def construct[R](using t: SQLTable[R])(r: Row): R =
  t.prod.constructor(TableRow(r, t.prod.elements))

def construct[R](b: Branch[R, SQLType])(r: Row): R =
  b.upcast(b.constructor(TableRow(r, b.elements)))

def attribValues[R](r: R)(using t: SQLTable[R]) = colValues(r, t.prod.elements)

def attribValues[R](bv: BranchValue[R, SQLType]) = colValues(bv.value, bv.branch.elements)

type Insertable[R] = SQLTable[R]|SQLModel[R]

def declare[R](using c: Insertable[R]): Connection ?=> Int = 
  c match
    case given SQLTable[R] => declareTemplate(tableName, attribNames).execute()
    case SQLModel(sum) =>
      var n = 0
      for b <- sum.branches
      do n += declareTemplate(tableName(b), attribNames(b)).execute()
      n

def insert[R](r: R)(using c: Insertable[R]): Connection ?=> Int = 
  c match
    case given SQLTable[R] => insertTemplate(tableName, attribNames, attribValues(r)).execute()
    case SQLModel(sum) =>
      val bv = sum.cast(r)
      val b = bv.branch
      insertTemplate(tableName(b), attribNames(b), attribValues(bv)).execute()

def insertAll[R](rs: Generator[R])(using c: Insertable[R]): Connection ?=> Int = 
  var n = 0
  for c <- chunked(rs, 2000)
  do n += insertAll(c)
  n

def insertAll[R](rs: Iterable[R])(using c: Insertable[R]): Connection ?=> Int = 
  c match
    case SQLTable(prod) => repeatedInsert(rs, prod.label, prod.elements)
    case SQLModel(sum) =>
      var n = 0
      val grps = rs.map(sum.cast(_)).groupBy(_.branch.ordinal)
      for (_, bvs) <- grps
      do
        val b = bvs.head.branch
        val vs = for bv <- bvs yield bv.value.asInstanceOf[b.B]
        n += repeatedInsert[b.B](vs, b.label, b.elements)
      n

def select[R](using c: Insertable[R]): Connection ?=> Generator[R] = select(selectTemplate)(using c)

def select[R](template: SelectTemplate)(using c: Insertable[R]): Connection ?=> Generator[R] =
  c match
    case given SQLTable[R] =>
      for r <- template(tableName, attribNames).results
      yield construct(r)
    case SQLModel(sum) =>
      for
        b <- Generator.from(sum.branches)
        q = template(tableName(b), attribNames(b))
        r <- q.results
      yield
        construct(b)(r)

def selectBranch[R](label: String, template: SelectTemplate = selectTemplate)(using t: SQLModel[R]): Connection ?=> Generator[R] = 
  val b = t.sum.branches.find(_.label == label).get
  val q = template(tableName(b), attribNames(b))
  for r <- q.results yield construct(b)(r)

extension(rs: Connection ?=> Generator[Row])
  def as[R](using SQLTable[R]): Connection ?=> Generator[R] = 
    for r <- rs yield construct(r)
