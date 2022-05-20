package qeduce
package classify

import geny.Generator
import models.{SQLTable, SQLModel}
import scala.collection.mutable.ArrayBuffer
type QueryTemplate = Seq[Term[?]] => Query

final class Key[T]()

enum Variable[T]:
  case Table(struct: SQLTable[T])
  case Union(struct: SQLModel[T])
  case Buffer(key: Key[T])

enum Ante[T]:
  case RefAnte(reference: Variable[T])
  case QueryAnte(query: QueryTemplate, struct: SQLTable[T])
  case StreamAnte(stream: Generator[T])
  case IndexAnte[S, T](post: S, rule: Rule[S, T]) extends Ante[T]
  case CountAnte[S, T](base: Ante[S], f: Int => T) extends Ante[T]
  case AggregateAnte[S, T](base: Ante[S], f: Generator[S] => T) extends Ante[T]

enum Formula[S, T]:
  case Projection(ante: Ante[T], struct1: SQLTable[S], struct2: SQLTable[T])
  case Mapping(ante: Ante[T], f: T => S)
  case Expansion(ante: Ante[T], f: T => Generator[S])
  case Inverse(ante: Ante[T], rule: Rule[T, S])

case class Rule[S, T](lhs: Variable[S], rhs: Formula[S, T])
case class Program(stmts: IndexedSeq[Rule[?, ?]|Variable[?]])

class ProgramBuilder(val stmnts: ArrayBuffer[Rule[?, ?]|Variable[?]]):
  def add[T](v: Variable[T]) = 
    stmnts.addOne(v); v
  def add[S, T](r: Rule[S, T]) = 
    stmnts.addOne(r); r

def program[A](body: ProgramBuilder ?=> Unit): Program =
  val b = ProgramBuilder(ArrayBuffer.empty)
  body(using b)
  Program(b.stmnts.toIndexedSeq)

def table[T](using b: ProgramBuilder, t: SQLTable[T]): Variable[T] = b.add(Variable.Table(t))
def union[T](using b: ProgramBuilder, t: SQLModel[T]): Variable[T] = b.add(Variable.Union(t))
def buffer[T](using b: ProgramBuilder): Variable[T] = b.add(Variable.Buffer[T](Key()))
def query[T](query: QueryTemplate)(using t: SQLTable[T]) = Ante.QueryAnte(query, t)
def stream[T](generator: Generator[T]) = Ante.StreamAnte(generator)

extension[T](ante: Ante[T]|Variable[T])
  def project[S](using s: SQLTable[S], t: SQLTable[T]) = Formula.Projection(ante.asAnte, s, t)
  def map[S](f: T => S) = Formula.Mapping(ante.asAnte, f)
  def expand[S](f: T => Generator[S]) = Formula.Expansion(ante.asAnte, f)
  def invert[S](rule: Rule[T, S]) = Formula.Inverse(ante.asAnte, rule)
  def asAnte: Ante[T] =
    ante match
      case a: Ante[T] => a
      case v: Variable[T] => Ante.RefAnte(v)

extension[S](lhs: Variable[S])(using b: ProgramBuilder)
  def :-[T](rhs: Formula[S, T]) = b.add(Rule(lhs, rhs))

object example:

  case class Student(name: String, age: Int) derives SQLTable
  case class Teacher(name: String) derives SQLTable
  case class Age(age: Int) derives SQLTable

  program {
    val students = table[Student]
    val ages = buffer[Age]
    val byAge = ages :- students.project
    val byAge2 = ages :- students.map { case Student(_, age) => Age(age) }
  }
