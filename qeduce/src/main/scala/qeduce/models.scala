package qeduce

/**
 * Domain models as ADTs with corresponding database schemas.
 */ 
package models

import summit.{SumType, ProductType, Element}
import java.sql.Connection
import geny.Generator
import qeduce.helpers._
import scala.CanEqual.derived

export qeduce.helpers.{SelectTemplate, allRows}

type SQLCapable[R] = SQLTable[R]|SQLModel[R]
type SQLNest[R] = SQLTable[R]|SQLType[R]

case class SQLTable[R](prod: ProductType[R, SQLType])
object SQLTable:
  def derived[R](using prod: ProductType[R, SQLType]): SQLTable[R] = SQLTable(prod)

case class SQLModel[R](sum: SumType[R, SQLType])
object SQLModel:
  def derived[R](using sum: SumType[R, SQLType]): SQLModel[R] = SQLModel(sum)

case class SQLJoint[R](prod: ProductType[R, SQLNest])
object SQLJoint:
  def derived[R](using prod: ProductType[R, SQLNest]): SQLJoint[R] = SQLJoint(prod)
  given[R <: Tuple](using prod: ProductType[R, SQLNest]): SQLJoint[R] = SQLJoint(prod)

def tableName[R](using t: SQLTable[R]) = Query(t.prod.label)

def attribNames[R](using t: SQLTable[R]): Query = colNames(t.prod.elements)

def construct[R](using t: SQLTable[R])(r: Row): R =
  t.prod.constructor(RowProduct(r, t.prod.elements, None))

def constructQualified[R](using t: SQLTable[R])(r: Row, prefix: String): R =
  t.prod.constructor(RowProduct(r, t.prod.elements, Some(prefix)))

def qualifiedNames[R](using t: SQLJoint[R]): Query = 
  val ls =
    for 
      e <- t.prod.elements
      l <- e.typeclass match
        case SQLTable(nest) => 
          for d <- nest.elements
          yield s"${e.label}.${d.label}"
        case d: SQLType[e.A] => Seq(e.label)
    yield l

  Query(ls.mkString(", "))

def jointConstruct[R](using t: SQLJoint[R])(r: Row): R =
  t.prod.constructor(JointProduct(r, t.prod.elements))

class JointProduct[B](row: Row, elements: IndexedSeq[Element[B, SQLNest]]) extends Product:
  def productElement(ix: Int): Any = 
    val e = elements(ix)
    e.typeclass match
      case given SQLTable[e.A] => constructQualified(row, e.label)
      case given SQLType[e.A] => row(e.label)
    
  def productArity: Int = elements.size
  def canEqual(other: Any) = false

def attribValues[R](r: R)(using t: SQLTable[R]): Query = colValues(r, t.prod.elements)

def declare[R](using c: SQLCapable[R]): Connection ?=> Int = 
  c match
    case SQLTable(prod) => createTemplate(prod.label, prod.elements).execute()
    case SQLModel(sum) =>
      var n = 0
      for b <- sum.branches
      do n += createTemplate(b.label, b.elements).execute()
      n

def insert[R](r: R)(using c: SQLCapable[R]): Connection ?=> Int = 
  c match
    case SQLTable(prod) => insertTemplate(r, prod.label, prod.elements).execute()
    case SQLModel(sum) =>
      val bv = sum.cast(r)
      insertTemplate(bv.value, bv.branch.label, bv.branch.elements).execute()

def insertAll[R](rs: Iterable[R])(using c: SQLCapable[R]): Connection ?=> Int = 
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

def select[R](using c: SQLCapable[R]): Connection ?=> Generator[R] = select(allRows)(using c)

def select[R](template: SelectTemplate)(using c: SQLCapable[R]): Connection ?=> Generator[R] =
  c match
    case given SQLTable[R] =>
      for r <- template(attribNames, tableName).results
      yield construct(r)
    case SQLModel(sum) =>
      for
        b <- Generator.from(sum.branches)
        q = expandSelect(b.label, b.elements, template)
        r <- q.results
      yield
        b.upcast(b.constructor(RowProduct(r, b.elements, None)))

def selectBranch[R](label: String, template: SelectTemplate = allRows)(using t: SQLModel[R]): Connection ?=> Generator[R] = 
  val b = t.sum.branches.find(_.label == label).get
  val q = expandSelect(label, b.elements, template)
  q.results.map(r => b.upcast(b.constructor(RowProduct(r, b.elements, None))))

extension(rs: Connection ?=> Generator[Row])
  def as[R](using SQLTable[R]): Connection ?=> Generator[R] = 
    rs.map(construct)
