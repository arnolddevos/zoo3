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

trait IsProjection[R, S]:
  def apply(r: R): S

object IsProjection

enum Var[R]:
  case Stored(table: Name, struct: Struct[R])
  case Resident(buffer: ArrayBuffer[R])

enum Value[R]:
  case QueryValue(query: Query, struct: Struct[R])
  case GeneratorValue(generator: Connection => Generator[R])

enum Expr[R]:
  case RefNode(subject: Var[R])
  case ValueNode(value: Value[R])
  case AggregateNode[R, S, T](group: Group[R, S], aggregate: (S, Value[R]) => T) extends Expr[T]
  case CountNode[R, S, T](group: Group[R, S], aggregate: (S, Int) => T) extends Expr[T]
  case ProjectNode[R, S](group: Group[R, S]) extends Expr[S]
  case DistinctNode(expr: Expr[R])

enum Group[R, S]:
  case Projection(expr: Expr[R], witness: IsProjection[R, S], struct: Struct[S])
  case Mapping(expr: Expr[R], map: R => S)

enum Rule:
  case Imply[R](head: Var[R], body: Expr[R])

case class Program( rules: ArraySeq[Rule])

import Var._, Value._, Expr._, Group._, Rule._

def suspend[R](effect: Connection ?=> Generator[R]): Value[R] =
  GeneratorValue(connection => effect(using connection))

def release[R](value: Value[R]): Connection ?=> Generator[R] =
  value match
    case GeneratorValue(generator) => generator(summon[Connection])
    case QueryValue(query, struct) => sqlTable(struct).select((_, _) => query)

def interpret[R](e: Expr[R]): Value[R] =
  e match
    case RefNode(Stored(table, struct)) => 
      suspend(
        sqlTable(struct).select(
          (cols, _) => allRows(cols, Query(table))
        )
      )
    case RefNode(Resident(buffer)) => suspend(Generator.from(buffer))
    case ValueNode(value) => value
    case AggregateNode(group, aggregate) => 
      suspend(
        for (s, g) <- release(interpret(group) )
        yield aggregate(s, g)
      )
    case CountNode(group, aggregate) => 
      suspend(
        for (s, g) <- release(interpret(group))
        yield aggregate(s, release(g).foldLeft(0)((n, _) => n+1))
      )
    case ProjectNode(group) =>
      suspend(
        for (s, _) <- release(interpret(group))
        yield s
      )
    case DistinctNode(expr) => 
      suspend(
        Generator.from(release(interpret(expr)).toBuffer.distinct)
      )

def interpret[R, S](g: Group[R, S]): Value[(S, Value[R])] =
  g match
    case Projection(expr, project, struct) => 
      suspend {
        val a = release(interpret(expr)).toBuffer.groupBy(project(_))
        val b = a.map((s, rs) => (s, suspend(Generator.from(rs))))
        Generator.from(b.toBuffer)
      }

    case Mapping(expr, map) => 
      suspend {
        val a = release(interpret(expr)).toBuffer.groupBy(map)
        val b = a.map((s, rs) => (s, suspend(Generator.from(rs))))
        Generator.from(b.toBuffer)
      }

def interpret(p: Program): Connection ?=> Unit =
  for r <- p.rules
  do
    r match
      case Imply(Stored(table, struct), expr) => 
        for r <- release(interpret(expr))
        do sqlTable(struct).insert(r)
      case Imply(Resident(buffer), expr) => 
        for r <- release(interpret(expr))
        do buffer += r

def sqlTable[R](struct: Struct[R]): SQLTable[R] =
  struct match 
    case given ProductType[R, SQLType] => SQLTable.derived
    case given SumType[R, SQLType] => SQLModel.derived

def table[R](using struct: Struct[R]) = Stored(struct.label, struct)
def table[R](name: Name)(using struct: Struct[R]) = Stored(name, struct)
def buffer[R] = Resident(ArrayBuffer())

extension[R](head: Var[R])
  def :=(body: Expr[R]) = Imply(head, body)
  def apply() = RefNode(head) 

extension[R](expr: Expr[R])
  def project[S](using struct: Struct[S], witness: IsProjection[R, S]) =
    Projection(expr, witness, struct)
  def groupBy[S](f: R => S) = Mapping(expr, f)
  def distinct = DistinctNode(expr)

extension[R, S](group: Group[R, S])
  def count[T](f: (S, Int) => T) = CountNode(group, f)
  def apply[T](f: (S, Connection ?=> Generator[R]) => T) = AggregateNode(group, (s, v) => f(s, release(v)))
  def distinct = ProjectNode(group)

def lift[R](generator: Connection ?=> Generator[R]) = ValueNode(suspend(generator))
def lift[R](values: Iterable[R]) = ValueNode(suspend(Generator.from(values)))
def lift[R](using struct: Struct[R])(query: Query) = ValueNode(QueryValue(query, struct))
