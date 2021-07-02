package qeduce
package parsable

import summit.{ProductType, Element, trace}

trait ParsableField[A]:
  def parse: String => Option[A]

object ParsableField:
  given ParsableField[Int] with
    val parse = _.toIntOption

  given ParsableField[Long] with
    val parse = _.toLongOption

  given ParsableField[Double] with
    val parse = _.toDoubleOption

  given ParsableField[Boolean] with
    val parse = _.toBooleanOption

  given ParsableField[String] with
    val parse = Option(_)

def parse[R](fields: IndexedSeq[String])(using t: Parsable[R]) = t.parse(fields)
def parseTSV[R](line: String)(using Parsable[R]) = parse(line.split('\t').toIndexedSeq)

trait Parsable[R]:
  def parse(fields: IndexedSeq[String]): Option[R]

object Parsable:
  def derived[R](using prod: ProductType[R, ParsableField]): Parsable[R] =
    trace(s"deriving Parsable[${prod.label}]")
    new Parsable[R]:
      def parse(fields: IndexedSeq[String]): Option[R] =
        val vs: IndexedSeq[Any] =
          for 
            (e, f) <- prod.elements.zip(fields)
            v <- e.typeclass.parse(f)
          yield  v
        if vs.size != prod.elements.size then None
        else Some(prod.constructor(SeqProduct(vs)))

class SeqProduct(seq: IndexedSeq[Any]) extends Product:
  def productElement(ix: Int): Any = seq(ix)
  def productArity: Int = seq.size
  def canEqual(other: Any) = false
