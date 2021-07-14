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

enum Expr[R]:
  case RefNode(subject: Var[R])
  case QueryNode(query: SelectTemplate, struct: Struct[R])
  case GenerateNode(generator: Connection => Generator[R])
  case AggregateNode[R, S, T](group: Group[R, S], aggregate: (S, Connection ?=> Generator[R]) => T) extends Expr[T]
  case CountNode[R, S, T](group: Group[R, S], aggregate: (S, Int) => T) extends Expr[T]
  case ProjectNode[R, S](group: Group[R, S]) extends Expr[S]
  case DistinctNode(expr: Expr[R])

enum Group[R, S]:
  case Projection(expr: Expr[R], witness: IsProjection[R, S], struct: Struct[S])
  case Mapping(expr: Expr[R], map: R => S)

enum Rule:
  case Imply[R](head: Var[R], body: Expr[R])

case class Program( rules: ArraySeq[Rule])

import Var._, Expr._, Group._, Rule._

def interpret[R](e: Expr[R]): Connection ?=> Generator[R] =
  e match
    case RefNode(Stored(table, struct)) => sqlTable(struct).select(allRows)
    case RefNode(Resident(buffer)) => Generator.from(buffer)
    case QueryNode(query, struct) => sqlTable(struct).select(query)
    case GenerateNode(generator) => generator(summon[Connection])
    case AggregateNode(group, aggregate) => 
      for (s, g) <- interpret(group) 
      yield aggregate(s, g)
    case CountNode(group, aggregate) => 
      for (s, g) <- interpret(group)
      yield aggregate(s, g.foldLeft(0)((n, _) => n+1))
    case ProjectNode(group) =>
      for (s, _) <- interpret(group)
      yield s
    case DistinctNode(expr) => Generator.from(interpret(expr).toBuffer.distinct)

def interpret[R, S](g: Group[R, S]): Connection ?=> Generator[(S, Connection ?=> Generator[R])] =
  g match
    case Projection(expr, project, struct) => 
      val a = interpret(expr).toBuffer.groupBy(project(_))
      val b = a.map[(S, Connection ?=> Generator[R])]((s, rs) => (s, Generator.from(rs))).toBuffer
      Generator.from(b)

    case Mapping(expr, map) => 
      val a = interpret(expr).toBuffer.groupBy(map)
      val b = a.map[(S, Connection ?=> Generator[R])]((s, rs) => (s, Generator.from(rs))).toBuffer
      Generator.from(b)

def interpret(p: Program): Connection ?=> Unit =
  for r <- p.rules
  do
    r match
      case Imply(Stored(table, struct), expr) => 
        for r <- interpret(expr)
        do sqlTable(struct).insert(r)
      case Imply(Resident(buffer), expr) => 
        for r <- interpret(expr)
        do buffer += r

def sqlTable[R](struct: Struct[R]): SQLTable[R] =
  struct match 
    case given ProductType[R, SQLType] => SQLTable.derived
    case given SumType[R, SQLType] => SQLModel.derived

def table[R](using struct: Struct[R]) = Stored(struct.label, struct)
def buffer[R] = Resident(ArrayBuffer())

extension[R](head: Var[R])
  def :=(body: Expr[R]) = Imply(head, body)

extension[R](expr: Expr[R])
  def project[S](using struct: Struct[S], witness: IsProjection[R, S]) =
    Projection(expr, witness, struct)
  def groupBy[S](f: R => S) =
    Mapping(expr, f)

extension[R, S](group: Group[R, S])
  def count[T](f: (S, Int) => T) = CountNode(group, f)
  def apply[T](f: (S, Connection ?=> Generator[R]) => T) = AggregateNode(group, f)

def select[R](subject: Var[R]) = RefNode(subject)
def select[X, R](group: Group[X, R]) = ProjectNode(group)
def select[R](generator: Connection ?=> Generator[R]) = GenerateNode(c => generator(using c))
def select[R](values: Iterable[R]) = GenerateNode(_ => Generator.from(values))
def select[R](using struct: Struct[R])(query: SelectTemplate) = QueryNode(query, struct)
def distinct[R](expr: Expr[R]) = DistinctNode(expr)