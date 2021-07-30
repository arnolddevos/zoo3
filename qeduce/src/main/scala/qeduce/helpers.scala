package qeduce
package helpers

import summit.Element
import java.sql.Connection

type SelectTemplate = (Query, Seq[Term[?]]) => Query

def selectTemplate(source: Query, attribs: Seq[Term[?]]) = 
  sql"select" ~ attribs ~ sql"from" ~ source 

def declareTemplate(name: Query, attribs: Seq[Term[?]]) = 
  sql"create table if not exists" ~ name ~ sql"(" ~ attribs ~ sql")"

def insertTemplate(name: Query, attribs: Seq[Term[?]], values: Seq[Param]) = 
  sql"insert into" ~ name ~ sql"("  ~ attribs ~ sql") values (" ~ values ~ sql")"

def colName[B](e: Element[B, SQLType]) = Term[e.A](e.label)

def colNames[B](elements: IndexedSeq[Element[B, SQLType]]): Seq[Term[?]] =
  for e <- elements yield colName(e)

def colValues[R](r: R, elements: IndexedSeq[Element[R, SQLType]]): Seq[Param] =
  for e <- elements yield Param(e.pick(r))(using e.typeclass)

def repeatedInsert[R](rs: Iterable[R], label: String, elements: IndexedSeq[Element[R, SQLType]]): Connection ?=> Int =
  var n = 0
  if ! rs.isEmpty then
    val qy = insertTemplate(Query(label), colNames(elements), colValues(rs.head, elements))
    for st <- qy.prepare()
    do 
      n += st.executeUpdate()
      for b <- rs.tail
      do
        for e <- elements
        do e.typeclass.inject(st, e.index+1, e.pick(b))
        n += st.executeUpdate()
  n

class TableRow[R](row: Row, elements: IndexedSeq[Element[R, SQLType]]) extends Product:
  def productElement(ix: Int): Any = 
    val e = elements(ix)
    row(colName(e))(using e.typeclass)
  def productArity: Int = elements.size
  def canEqual(other: Any) = false
