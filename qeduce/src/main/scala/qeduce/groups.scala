package qeduce
package groups

import summit.{ProductType, SumType, SumOrProductType}
import scala.collection.mutable.ArrayBuffer
import java.sql.Connection
import geny.Generator
import scala.collection.immutable.ArraySeq

type Name = String
type Struct[R] = SumOrProductType[R, SQLType]

enum Var[R]:
  case Stored(table: Name, struct: Struct[R])
  case Resident(buffer: ArrayBuffer[R], struct: Struct[R])

enum Expr[R]:
  case RefNode(subject: Var[R])
  case QueryNode(query: Query, struct: Struct[R])
  case GenerateNode(gen: Connection => Generator[R], struct: Struct[R])
  case AggregateNode[R, S, T](group: Group[R, S], aggr: (S, Expr[R]) => T) extends Expr[T]
  case CountNode[R, S, T](group: Group[R, S], aggr: (S, Int) => T) extends Expr[T]
  case ProjectNode[R, S](group: Group[R, S]) extends Expr[S]
  case DistinctNode(expr: Expr[R])

enum Group[R, S]:
  case Projection(expr: Expr[R], struct: Struct[S])
  case Mapping(expr: Expr[R], map: R => S, struct: Struct[S])

enum Rule:
  case Implication[R](head: Var[R], body: Expr[R])

case class Program( parts: ArraySeq[Rule])


