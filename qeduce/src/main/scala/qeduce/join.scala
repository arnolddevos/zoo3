package qeduce
package models
package joint

import summit.{SumType, ProductType, Element}
import java.sql.Connection
import geny.Generator

type InnerType[T] = SQLTable[T]|SQLType[T]

case class SQLJoin[R](prod: ProductType[R, InnerType])

object SQLJoin:
  def derived[R](using prod: ProductType[R, InnerType]): SQLJoin[R] = SQLJoin(prod)
  given[R <: Tuple](using prod: ProductType[R, InnerType]): SQLJoin[R] = SQLJoin(prod)

def colName[B](prefix: String, e: Element[B, SQLType]) = Term[e.A](s"$prefix.${e.label}")

def colName[B](e: Element[B, InnerType]) = Term[e.A](e.label)

def attribNames[R](using t: SQLJoin[R]): Seq[Term[?]] = 
  for 
    e <- t.prod.elements
    l <- e.typeclass match
      case SQLTable(inner) => for d <- inner.elements yield colName(e.label, d)
      case _: SQLType[e.A] => Seq(colName(e))
  yield l


def construct[R](using t: SQLJoin[R])(r: Row): R =
  t.prod.constructor(JoinRow(r, t.prod.elements))

class JoinRow[B](row: Row, elements: IndexedSeq[Element[B, InnerType]]) extends Product:
  def productElement(ix: Int): Any = 
    val e = elements(ix)
    e.typeclass match
      case SQLTable(inner) => inner.constructor(InnerRow(row, e.label, inner.elements))
      case d: SQLType[e.A] => row(colName(e))(using d)
    
  def productArity: Int = elements.size
  def canEqual(other: Any) = false

class InnerRow[R](row: Row, prefix: String, elements: IndexedSeq[Element[R, SQLType]]) extends Product:
  def productElement(ix: Int): Any = 
    val e = elements(ix)
    row(colName(prefix, e))(using e.typeclass)
  def productArity: Int = elements.size
  def canEqual(other: Any) = false
