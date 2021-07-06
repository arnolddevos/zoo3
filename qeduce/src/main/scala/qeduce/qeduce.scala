package qeduce

import java.sql.{ResultSet, PreparedStatement, SQLException, Connection, DriverManager}
import java.util.Properties
import scala.util.Try
import scala.util.control.NonFatal
import scala.collection.immutable.ArraySeq
import summit.trace
import geny.Generator
 
/**
 * A Query can be constructed by the sql string interpolator and 
 * executed against a JDBC Connection. 
 * 
 * Internally, it consists of N sql text parts and N-1 
 * interleaved, typed parameter values.
 */
trait Query extends SQLRepr:
  query =>

  def parts: Seq[String]
  def params: Seq[Param]

  def ~(other: Query): Query =
    val a = parts dropRight 1
    val b = parts.last + " " + other.parts.head
    val c = other.parts drop 1
    val d = params ++ other.params
    new Query:
      val parts = (a :+ b) ++ c
      val params = d

  def ~[A](a: A)(using t: SQLType[A]): Query =
    this ~ Query(Param(a))

  def ~[A](term: SQLTerm[A]): Query =
    this ~ Query(term.name)

  def +(other: Query) = CompoundQuery(ArraySeq(this, other))
    
  override def toString = parts.zip(params).map(_ + _).mkString + parts.last

  /**
   * Prepare, execute and close a JDBC statement for this Query
   * using a Connection.
   * 
   * Pass the executed statement to a fn. It may not be used outside this fn.
   */
  def apply(): Connection ?=> Generator[PreparedStatement] =
    new Generator[PreparedStatement]:
      def generate(effect: PreparedStatement => Generator.Action): Generator.Action =
        trace(query.toString)
        val st = summon[Connection].prepareStatement(parts.mkString("?"))
        try
          for (p, i) <- params.zipWithIndex
          do p.sqlType.inject(st, i+1, p.value)
          effect(st)
        finally
          st.close

object Query:
  /**
   * Construct a static Query fragment explicitly.
   */
  def apply(text: String): Query =
    new Query:
      val parts = Seq(text)
      val params = Seq()

  /**
   * Construct a Query fragment consisting of a parameter.
   */
  def apply(param: Param): Query =
    new Query:
      val parts = Seq("", "")
      val params = Seq(param)

  /**
   * Construct comma separated parameters.
   */
  def apply(ps: Seq[Param]): Query =
    new Query:
      val parts = 
        ps.size match
          case 0 => Seq("")
          case 1 => Seq("", "")
          case n => Seq("") ++ Seq.fill(n-1)(", ") ++ Seq("")
      val params = ps

  val empty: Query = apply("")

  def resultGenerator(st: PreparedStatement) =
    new Generator[Row]:
      def generate(f: Row => Generator.Action): Generator.Action =
        var a: Generator.Action = Generator.Continue
        val cursor = st.executeQuery()
        try
          while a == Generator.Continue && cursor.next() 
          do a = f(Row(cursor))
          a
        finally
          cursor.close
   
/**
 * The `sql` string interpolator.
 */
extension( sc: StringContext)
  def sql( ps: Param* ): Query = 
    new Query:
      val parts = sc.parts
      val params = ps

/**
 * A concatenation of Query's.
 */       
class CompoundQuery(val queries: ArraySeq[Query]) extends SQLRepr:

  def +(query: Query) = CompoundQuery(queries :+ query)
  def +(other: CompoundQuery) = CompoundQuery(queries ++ other.queries)
  
  def apply(): Connection ?=> Generator[PreparedStatement] =
    for 
      q <- Generator.from(queries)
      s <- q()
    yield s

  override def toString = queries.mkString(";\n")

/**
 * A Query or CompoundQuery represents some SQL.
 */ 
trait SQLRepr:

  /**
   * Generate JDBC statements, pending a connection.
   */ 
  def apply(): Connection ?=> Generator[PreparedStatement]

  /**
   * Execute a the statements as an update or data definition and return the update count.
   */
  def update: Connection ?=> Int = 
    var ig = 0
    for st <- apply()
    do ig += st.executeUpdate()
    ig

  /**
   * Execute the statments as queries and return a Generator of Rows
   */   
  def results: Connection ?=> Generator[Row] = 
    for 
      st <- apply()
      row <- Query.resultGenerator(st)
    yield row

/**
 * Connect to a database and provide the connection as a given to a context fn.
 * The Connection may not be used outside the fn. 
 */    
def connect[A](url: String, props: Properties = new Properties)( effect: Connection ?=> A ): A =
  val c = DriverManager.getConnection(url, props)
  c.setAutoCommit(true)
  try
    effect(using c)
  finally
    c.close()

def transaction[A](effect: Connection ?=> A)(using c: Connection): A =
  if ! c.getAutoCommit then effect(using c)
  else
    c.setAutoCommit(false)
    try 
      try 
        val a = effect(using c)
        c.commit()
        a
      catch
        case NonFatal(e) =>
          c.rollback()
          throw e
    finally
      c.setAutoCommit(true)

/**
 * A Param is a parameter to a query comprising a value 
 * and a corresponding SQLType.
 */
trait Param:
  type T 
  def sqlType: SQLType[T]
  def value: T
  override def toString = sqlType.display(value)

object Param:
  /**
   * Construct a Param expicitly
   */
  def apply[A](a: A)(using t: SQLType[A]): Param =
    new:
      type T = A
      val sqlType = t
      val value = a
    
  /**
   * Implicitly convert a value to a Param.
   */
  given magnet[A](using t: SQLType[A]): Conversion[A, Param] with
    def apply(a: A) = Param(a)

/**
 * An SQLTerm is an SQL identifier and corresponding type.   
 */ 
final class SQLTerm[A](val name: String)

/**
 * A Row is a restricted view of a ResultSet.
 */
opaque type Row = ResultSet

object Row:
  def apply(r: ResultSet): Row = r

  extension(r: Row)
    /**
     * Extract a value of an explicit type from a Row by column name or throw SQLException.
     * 
     * A type-dependent default is return for SQL Null.  To detect an SQL Null, 
     * extract an Option of the underlying value type.
     */
    def apply[A](name: String)(using t: SQLType[A]): A = t.extract(r, name)
    /**
     * Extract a value of designated by a typed SQLTerm or throw SQLException.
     */
    def apply[A](term: SQLTerm[A])(using t: SQLType[A]): A = t.extract(r, term.name)
    /**
     * Extract a value from a Row by term as a Try.
     */
    def get[A](term: SQLTerm[A])(using t: SQLType[A]): Try[A] = Try(t.extract(r, term.name))

