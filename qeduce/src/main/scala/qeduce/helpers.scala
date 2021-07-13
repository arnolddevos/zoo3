package qeduce
package helpers

import summit.{SumType, ProductType, SumOrProductType, Branch, BranchValue, Element, trace}
import java.sql.{PreparedStatement, Connection}
import geny.Generator

type SelectTemplate = (Query, Query) => Query
val allRows: SelectTemplate = sql"select" ~ _ ~ sql"from" ~ _ 

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

def expandSelect[B](label: String, elements: IndexedSeq[Element[B, SQLType]], template: SelectTemplate): Query =
  template(colNames(elements), Query(label))

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
