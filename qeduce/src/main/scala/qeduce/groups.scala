package qeduce
package groups
import qeduce.models.{Insertable, SQLTable, SelectTemplate}
import qeduce.models.joins.{SQLJoin, Selectable, attribNames, construct}

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ArraySeq
import java.sql.Connection
import geny.Generator

class Key[R]
type Residence = [R] => Key[R] => ArrayBuffer[R]
type QueryTemplate = Seq[Term[_]] => Query
type JoinTemplate = (Query, Query, Seq[Term[_]]) => Query

enum Var[R]:
  case Stored(struct: Insertable[R])
  case Resident(buffer: Residence => ArrayBuffer[R])

enum Value[R]:
  case QueryValue(query: QueryTemplate, struct: Selectable[R])
  case GeneratorValue(generator: (Residence, Connection) => Generator[R])

enum Expr[R]:
  case RefNode(subject: Var[R])
  case ValueNode(value: Value[R])
  case UnaryNode[R, S](expr: Expr[R], trans: Generator[R] => Generator[S]) extends Expr[S]
  case BinaryNode[R, T, S](lhs: Expr[R], rhs: Expr[T], join: (Generator[R], Generator[T]) => Generator[S]) extends Expr[S]
  case SelectNode[R, S](expr: Expr[R], query: SelectTemplate, struct: Selectable[S]) extends Expr[S]
  case JoinNode[R, T, S](lhs: Expr[R], rhs: Expr[T], query: JoinTemplate, struct: Selectable[S]) extends Expr[S]
  case AggregateNode[R, S, T](expr: Expr[R], group: Grouping[R, S], aggregate: (S, Value[R]) => T) extends Expr[T]
  case CountNode[R, S](expr: Expr[R], group: Grouping[R, S]) extends Expr[Counted[S]]
  case ProjectNode[R, S](expr: Expr[R], group: Grouping[R, S]) extends Expr[S]
  case DistinctNode(expr: Expr[R])

enum Rule:
  case Imply[R](head: Var[R], body: Expr[R])

case class Program( rules: ArraySeq[Rule])

enum Grouping[R, S]:
  case Projection(struct: SQLTable[S])
  case Mapping(map: R => S)

case class Counted[S](image: S, value: Int)

object Counted:
  given[S: SQLTable]: SQLJoin[Counted[S]] = SQLJoin.derived

import Var._, Value._, Expr._, Grouping._, Rule._

def suspend[R](effect: Residence ?=> Connection ?=> Generator[R]): Value[R] =
  GeneratorValue((residence, connection) => effect(using residence)(using connection))

def release[R](value: Value[R]): Residence ?=> Connection ?=> Generator[R] =
  value match
    case GeneratorValue(generator) => generator(summon[Residence], summon[Connection])
    case QueryValue(query, struct) => query(attribNames(using struct)).results.map(construct(using struct))

def inject[R](value: Value[R], template: SelectTemplate): Residence ?=> Connection ?=> QueryTemplate =
  value match
    case GeneratorValue(generator) => ???
    case QueryValue(query, struct) => template(query(attribNames(using struct)), _)

opaque type ProgramBuffer = ArrayBuffer[Rule]

def program(rules: ProgramBuffer ?=> Unit): Program = 
  val buffer: ProgramBuffer = ArrayBuffer()
  rules(using buffer)
  Program(ArraySeq.from(buffer))

def table[R](using struct: Insertable[R]) = Stored(struct)
def buffer[R] = 
  val k = Key[R]
  Resident(r => r(k))

extension[R](head: Var[R])
  def :=(body: Expr[R]) = Imply(head, body)
  def apply() = RefNode(head) 

extension[R](expr: Expr[R])
  def groupBy[S, T](f: R => S)(g: (S, Residence ?=> Connection ?=> Generator[R]) => T): Expr[T] = 
    groupBy(Mapping(f))(g)
  def groupBy[S, T](f: Grouping[R, S])(g: (S, Residence ?=> Connection ?=> Generator[R]) => T): Expr[T] = 
    AggregateNode(expr, f, (s, v) => g(s, release(v)))
  def count[S, T](f: R => S): Expr[Counted[S]] = count(Mapping(f))
  def count[S, T](g: Grouping[R, S]): Expr[Counted[S]] = CountNode(expr, g)
  def project[S](f: R => S): Expr[S] = project(Mapping(f))
  def project[S](f: Grouping[R, S]): Expr[S] = ProjectNode(expr, f)
  def distinct: Expr[R] = DistinctNode(expr)

def lift[R](generator: Residence ?=> Connection ?=> Generator[R]) = ValueNode(suspend(generator))
def lift[R](values: Iterable[R]) = ValueNode(suspend(Generator.from(values)))
def lift[R](using struct: Selectable[R])(query: QueryTemplate) = ValueNode(QueryValue(query, struct))

