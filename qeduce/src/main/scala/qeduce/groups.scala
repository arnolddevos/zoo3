package qeduce
package groups
import qeduce.models._

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ArraySeq
import java.sql.Connection
import geny.Generator

type Struct[R] = SQLCapable[R]
class Key[R]
type Residence = [R] => Key[R] => ArrayBuffer[R]

enum Var[R]:
  case Stored(struct: Struct[R])
  case Resident(buffer: Residence => ArrayBuffer[R])

enum Value[R]:
  case QueryValue(query: SelectTemplate, struct: Struct[R])
  case GeneratorValue(generator: (Residence, Connection) => Generator[R])

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

def suspend[R](effect: Residence ?=> Connection ?=> Generator[R]): Value[R] =
  GeneratorValue((residence, connection) => effect(using residence)(using connection))

def release[R](value: Value[R]): Residence ?=> Connection ?=> Generator[R] =
  value match
    case GeneratorValue(generator) => generator(summon[Residence], summon[Connection])
    case QueryValue(query, struct) => select(query)(using struct)

opaque type ProgramBuffer = ArrayBuffer[Rule]

def program(rules: ProgramBuffer ?=> Unit): Program = 
  val buffer: ProgramBuffer = ArrayBuffer()
  rules(using buffer)
  Program(ArraySeq.from(buffer))

def table[R](using struct: Struct[R]) = Stored(struct)
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
  def count[S, T](f: R => S)(g: (S, Int) => T): Expr[T] = count(Mapping(f))(g)
  def count[S, T](f: Grouping[R, S])(g: (S, Int) => T): Expr[T] = CountNode(expr, f, g)
  def project[S](f: R => S): Expr[S] = project(Mapping(f))
  def project[S](f: Grouping[R, S]): Expr[S] = ProjectNode(expr, f)
  def distinct: Expr[R] = DistinctNode(expr)

def lift[R](generator: Residence ?=> Connection ?=> Generator[R]) = ValueNode(suspend(generator))
def lift[R](values: Iterable[R]) = ValueNode(suspend(Generator.from(values)))
def lift[R](using struct: Struct[R])(query: SelectTemplate) = ValueNode(QueryValue(query, struct))

