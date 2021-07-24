package qeduce
package groups
package interpreter

import qeduce.models._
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ArraySeq
import java.sql.Connection
import geny.Generator

import Var._, Value._, Expr._, Grouping._, Rule._

def interpret[R](e: Expr[R]): Value[R] =
  e match
    case RefNode(Stored(struct)) => QueryValue(allRows, struct)
    case RefNode(Resident(buffer)) => suspend(Generator.from(buffer(summon[Residence])))
    case ValueNode(value) => value
    case AggregateNode(expr, group, aggregate) => 
      suspend(
        for (s, g) <- release(interpret(interpret(expr), group))
        yield aggregate(s, g)
      )
    case CountNode(expr, group, aggregate) => interpretCount(expr, group, aggregate)
    case ProjectNode(expr, group) => interpretProject(expr, group)
    case DistinctNode(expr) => 
      suspend(
        Generator.from(release(interpret(expr)).toBuffer.distinct)
      )

def interpretProject[R, S](expr: Expr[R], group: Grouping[R, S]): Value[S] = 
  val v = interpret(expr) 

  def fallback =
    suspend(
      for (s, _) <- release(interpret(v, group))
      yield s
    )

  v match
    case QueryValue(query, inner: SQLTable[R]) =>
      group match
        case Projection(map, alt, outer: SQLTable[S]) => 
          QueryValue((cs, n) => query(cs, tableName(using inner)) ~ sql"group by" ~ cs, outer)
        case _ => fallback
    case _ => fallback

def interpretCount[R, S, T](expr: Expr[R], group: Grouping[R, S], aggregate: (S, Int) => T): Value[T] = 
  val v = interpret(expr)

  def fallback =
    suspend(
      for (s, g) <- release(interpret(v, group))
      yield aggregate(s, release(g).foldLeft(0)((n, _) => n+1))
    )

  v match
    case QueryValue(query, inner: SQLTable[R]) =>
      group match
        case Projection(map, alt, outer: SQLTable[S]) => 
          QueryValue((cs, n) => query(cs ~ sql", count(*) as group_count", tableName(using inner)) ~ sql"group by" ~ cs, ???)
        case _ => fallback
    case _ => fallback
  


def interpret[R, S](v: Value[R], g: Grouping[R, S]): Value[(S, Value[R])] =
  g match
    case Projection(map, alt, struct) => 
      suspend {
        val a = release(v).toBuffer.groupBy(map(_))
        val b = a.map((s, rs) => (s, suspend(Generator.from(rs))))
        Generator.from(b.toBuffer)
      }

    case Mapping(map) => 
      suspend {
        val a = release(v).toBuffer.groupBy(map)
        val b = a.map((s, rs) => (s, suspend(Generator.from(rs))))
        Generator.from(b.toBuffer)
      }

def interpret(p: Program): Residence ?=> Connection ?=> Unit =
  for r <- p.rules
  do
    r match
      case Imply(r, e) => interpret(r, e)

def interpret[R](v: Var[R], e: Expr[R]): Residence ?=> Connection ?=> Unit =
  v match
    case Stored(lhs: SQLTable[R]) => 
      interpret(e) match
        case QueryValue(query, rhs: SQLTable[R]) => 
          sql"insert into" ~ tableName(using lhs) ~ sql"(" ~ attribNames(using lhs) ~ sql")" ~ 
          query(attribNames(using rhs), tableName(using rhs))
        case rs =>
          for r <- release(rs) do insert(r)(using lhs)

    case Stored(lhs) =>
      for r <- release(interpret(e)) do insert(r)(using lhs)

    case Resident(buffer) => 
      for r <- release(interpret(e))
      do buffer(summon[Residence]) += r