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

given[R, S](using isa: R <:< S): IsProjection[R, S] =
  new:
    def apply(r: R): S = isa(r)

enum Var[R]:
  case Stored(table: Name, struct: Struct[R])
  case Resident(buffer: ArrayBuffer[R], struct: Struct[R])

enum Expr[R]:
  case RefNode(subject: Var[R])
  case QueryNode(query: SelectTemplate, struct: Struct[R])
  case GenerateNode(gen: Connection => Generator[R], struct: Struct[R])
  case AggregateNode[R, S, T](group: Group[R, S], aggregate: (S, Connection => Generator[R]) => T) extends Expr[T]
  case CountNode[R, S, T](group: Group[R, S], aggregate: (S, Int) => T) extends Expr[T]
  case ProjectNode[R, S](group: Group[R, S]) extends Expr[S]
  case DistinctNode(expr: Expr[R])

enum Group[R, S]:
  case Projection(expr: Expr[R], witness: IsProjection[R, S], struct: Struct[S])
  case Mapping(expr: Expr[R], map: R => S, struct: Struct[S])

enum Rule:
  case Implication[R](head: Var[R], body: Expr[R])

case class Program( rules: ArraySeq[Rule])

import Var._, Expr._, Group._, Rule._

def interpret[R, S](g: Group[R, S]): Connection ?=> Generator[(S, Connection => Generator[R])] =
  g match
    case Projection(expr, project, struct) => 
      val a = interpret(expr).toBuffer.groupBy(project(_))
      val b = a.map((s, rs) => (s, (_: Connection) => Generator.from(rs))).toBuffer
      Generator.from(b)

    case Mapping(expr, map, struct) => 
      val a = interpret(expr).toBuffer.groupBy(map)
      val b = a.map((s, rs) => (s, (_: Connection) => Generator.from(rs))).toBuffer
      Generator.from(b)

def interpret[R](e: Expr[R]): Connection ?=> Generator[R] =
  e match
    case RefNode(Stored(table, struct)) => sqlTable(struct).select(allRows)
    case RefNode(Resident(buffer, _)) => Generator.from(buffer)
    case QueryNode(query, struct) => sqlTable(struct).select(query)
    case GenerateNode(generator, _) => generator(summon[Connection])
    case AggregateNode(group, aggregate) => 
      for (s, g) <- interpret(group) 
      yield aggregate(s, g)
    case CountNode(group, aggregate) => 
      for (s, g) <- interpret(group)
      yield aggregate(s, g(summon[Connection]).foldLeft(0)((n, _) => n+1))
    case ProjectNode(group) =>
      for (s, _) <- interpret(group)
      yield s
    case DistinctNode(expr) => Generator.from(interpret(expr).toBuffer.distinct)


def interpret(p: Program): Connection ?=> Unit =
  for r <- p.rules
  do
    r match
      case Implication(Stored(table, struct), expr) => 
        for r <- interpret(expr)
        do sqlTable(struct).insert(r)
      case Implication(Resident(buffer, _), expr) => 
        for r <- interpret(expr)
        do buffer += r



def sqlTable[R](struct: Struct[R]): SQLTable[R] =
  struct match 
    case given ProductType[R, SQLType] => SQLTable.derived
    case given SumType[R, SQLType] => SQLModel.derived
