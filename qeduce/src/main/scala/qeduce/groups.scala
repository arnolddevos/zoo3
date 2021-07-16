package qeduce
package groups
import qeduce.models._

import summit.{ProductType, SumType, SumOrProductType}
import scala.collection.mutable.ArrayBuffer
import java.sql.Connection
import geny.Generator
import scala.collection.immutable.ArraySeq

type Name = String
type Struct[R] = SumOrProductType[R, SQLType]

enum Var[R]:
  case Stored(struct: Struct[R])
  case Resident(buffer: ArrayBuffer[R])

enum Value[R]:
  case QueryValue(query: Query, struct: Struct[R])
  case GeneratorValue(generator: Connection => Generator[R])

enum Expr[R]:
  case RefNode(subject: Var[R])
  case ValueNode(value: Value[R])
  case AggregateNode[R, S, T](expr: Expr[R], group: Grouping[R, S], aggregate: (S, Value[R]) => T) extends Expr[T]
  case CountNode[R, S, T](expr: Expr[R], group: Grouping[R, S], aggregate: (S, Int) => T) extends Expr[T]
  case ProjectNode[R, S](expr: Expr[R], group: Grouping[R, S]) extends Expr[S]
  case DistinctNode(expr: Expr[R])

enum Rule:
  case Imply[R](head: Var[R], body: Expr[R])

case class Program( rules: ArraySeq[Rule])

enum Grouping[R, S]:
  case Projection(map: R => S, alt: SelectTemplate, struct: Struct[S])
  case Mapping(map: R => S)

import Var._, Value._, Expr._, Grouping._, Rule._

def suspend[R](effect: Connection ?=> Generator[R]): Value[R] =
  GeneratorValue(connection => effect(using connection))

def release[R](value: Value[R]): Connection ?=> Generator[R] =
  value match
    case GeneratorValue(generator) => generator(summon[Connection])
    case QueryValue(query, struct) => sqlTable(struct).select((_, _) => query)

def interpret[R](e: Expr[R]): Value[R] =
  e match
    case RefNode(Stored(struct)) => 
      suspend(
        sqlTable(struct).select(
          (cols, _) => allRows(cols, Query(struct.label))
        )
      )
    case RefNode(Resident(buffer)) => suspend(Generator.from(buffer))
    case ValueNode(value) => value
    case AggregateNode(expr, group, aggregate) => 
      suspend(
        for (s, g) <- release(interpret(expr, group) )
        yield aggregate(s, g)
      )
    case CountNode(expr, group, aggregate) => 
      suspend(
        for (s, g) <- release(interpret(expr, group))
        yield aggregate(s, release(g).foldLeft(0)((n, _) => n+1))
      )
    case ProjectNode(expr, group) =>
      suspend(
        for (s, _) <- release(interpret(expr, group))
        yield s
      )
    case DistinctNode(expr) => 
      suspend(
        Generator.from(release(interpret(expr)).toBuffer.distinct)
      )

def interpret[R, S](expr: Expr[R], g: Grouping[R, S]): Value[(S, Value[R])] =
  g match
    case Projection(map, alt, struct) => 
      suspend {
        val a = release(interpret(expr)).toBuffer.groupBy(map(_))
        val b = a.map((s, rs) => (s, suspend(Generator.from(rs))))
        Generator.from(b.toBuffer)
      }

    case Mapping(map) => 
      suspend {
        val a = release(interpret(expr)).toBuffer.groupBy(map)
        val b = a.map((s, rs) => (s, suspend(Generator.from(rs))))
        Generator.from(b.toBuffer)
      }

def interpret(p: Program): Connection ?=> Unit =
  for r <- p.rules
  do
    r match
      case Imply(Stored(struct), expr) => 
        for r <- release(interpret(expr))
        do sqlTable(struct).insert(r)
      case Imply(Resident(buffer), expr) => 
        for r <- release(interpret(expr))
        do buffer += r

def sqlTable[R](struct: Struct[R]): SQLTable[R] =
  struct match 
    case given ProductType[R, SQLType] => SQLTable.derived
    case given SumType[R, SQLType] => SQLModel.derived

def table[R](using struct: Struct[R]) = Stored(struct)
def buffer[R] = Resident(ArrayBuffer())

extension[R](head: Var[R])
  def :=(body: Expr[R]) = Imply(head, body)
  def apply() = RefNode(head) 

extension[R](expr: Expr[R])
  def groupBy[S, T](f: R => S)(g: (S, Connection ?=> Generator[R]) => T): Expr[T] = 
    groupBy(Mapping(f))(g)
  def groupBy[S, T](f: Grouping[R, S])(g: (S, Connection ?=> Generator[R]) => T): Expr[T] = 
    AggregateNode(expr, f, (s, v) => g(s, release(v)))
  def count[S, T](f: R => S)(g: (S, Int) => T): Expr[T] = count(Mapping(f))(g)
  def count[S, T](f: Grouping[R, S])(g: (S, Int) => T): Expr[T] = CountNode(expr, f, g)
  def project[S](f: R => S): Expr[S] = project(Mapping(f))
  def project[S](f: Grouping[R, S]): Expr[S] = ProjectNode(expr, f)
  def distinct: Expr[R] = DistinctNode(expr)

def lift[R](generator: Connection ?=> Generator[R]) = ValueNode(suspend(generator))
def lift[R](values: Iterable[R]) = ValueNode(suspend(Generator.from(values)))
def lift[R](using struct: Struct[R])(query: Query) = ValueNode(QueryValue(query, struct))
