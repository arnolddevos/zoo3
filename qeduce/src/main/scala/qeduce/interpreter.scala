package qeduce
package groups
package interpreter

import qeduce.models.{tableName, Insertable, SQLTable, SQLModel, selectTemplate, select, insert, insertAll}
import qeduce.models.joins.attribNames
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ArraySeq
import java.sql.Connection
import geny.Generator

import Var._, Value._, Expr._, Grouping._, Rule._

def interpret[R](e: Expr[R]): Value[R] =
  e match
    case RefNode(Stored(struct: SQLTable[R])) => QueryValue(selectTemplate(tableName(using struct), _), struct)
    case RefNode(Stored(struct: SQLModel[R])) => suspend(select(using struct))
    case RefNode(Resident(buffer)) => suspend(Generator.from(buffer(summon[Residence])))
    case ValueNode(value) => value
    case UnaryNode(expr, trans) => ???
    case BinaryNode(lhs, rhs, join) => ???
    case SelectNode(expr, query, struct) => ???
    case JoinNode(lhs, rhs, query, struct) => ???
    case AggregateNode(expr, group, aggregate) => ???
    case CountNode(expr, group) => ???
    case ProjectNode(expr, group) =>  ???
    case DistinctNode(expr) => ???

def interpret[R, S](v: Value[R], g: Grouping[R, S]): Value[(S, Value[R])] =
  g match
    case Projection(struct) => ???

    case Mapping(map) => 
      suspend {
        val a = release(v).toBuffer.groupBy(map)
        val b = a.map((s, rs) => (s, suspend(Generator.from(rs))))
        val c = b.toBuffer
        Generator.from(c)
      }

def interpret(p: Program): Residence ?=> Connection ?=> Unit =
  for r <- p.rules
  do
    r match
      case Imply(r, e) => interpret(r, e)

def interpret[R](lhs: Var[R], rhs: Expr[R]): Residence ?=> Connection ?=> Unit =
  lhs match
    case Stored(struct1: SQLTable[R]) => 
      interpret(rhs) match
        case QueryValue(query, struct2) => 
          sql"insert into" ~ tableName(using struct1) ~ sql"(" ~ attribNames(using struct1) ~ sql")" ~ 
          query(attribNames(using struct2))
        case rs =>
          insertAll(release(rs))(using struct1)

    case Stored(struct1) =>
      insertAll(release(interpret(rhs)))(using struct1)

    case Resident(buffer) => 
      val b = buffer(summon[Residence])
      for r <- release(interpret(rhs))
      do  b += r