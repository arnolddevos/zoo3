package qeduce
package parsable

import summit.{ProductType, Element, trace}

import java.time.LocalDate 
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalQueries.localDate
import scala.util.{Try, Success, Failure}

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

  given[A](using e: ParsableField[A]): ParsableField[Option[A]] with
    val parse = f => if f.isBlank then Some(None) else e.parse(f).map(Some(_))

  given ParsableField[LocalDate] with
    val parse = 
      field =>
        def loop(forms: List[DateTimeFormatter]): Option[LocalDate] =
          forms match
            case Nil => None
            case form :: alternates =>
              Try(form.parse(field, localDate)) match
                case Success(date) => Some(date)
                case Failure(_) => loop(alternates)
        end loop
        loop(allForms)

  val allForms = 
    List(
      DateTimeFormatter.ISO_LOCAL_DATE,
      DateTimeFormatter.ofPattern("d/M/yyyy"),
      DateTimeFormatter.ofPattern("d/M/yy"),
      DateTimeFormatter.ofPattern("d'.'M'.'yy"),
      DateTimeFormatter.ofPattern("d'.'M'.'yyyy")
    )

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
