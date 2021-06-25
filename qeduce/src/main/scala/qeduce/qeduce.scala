package qeduce

import java.sql.{ResultSet, PreparedStatement, SQLException, Connection, DriverManager}
import java.util.Properties
import scala.util.Try
import summit.trace
 import scala.util.control.NonFatal
 
/**
 * A Query can be constructed by the sql string interpolator and 
 * executed against a JDBC Connection. 
 * 
 * Internally, it consists of N sql text parts and N-1 
 * interleaved, typed parameter values.
 */
trait Query:
  def parts: Seq[String]
  def params: Seq[Param]

  def ~(other: Query): Query =
    val a = parts dropRight 1
    val b = parts.last + " " + other.parts.head
    val c = other.parts drop 1
    val d = params ++ other.params
    new:
      val parts = (a :+ b) ++ c
      val params = d

  def ~[A](a: A)(using t: SQLType[A]): Query =
    this ~ Query(Param(a))

  def ~[A](term: SQLTerm[A]): Query =
    this ~ Query(term.name)
    
  override def toString = parts.zip(params).map(_ + _).mkString + parts.last

  /**
   * Prepare, execute and close a JDBC statement for this Query
   * using a Connection.
   * 
   * Pass the executed statement to a fn. It may not be used outside this fn.
   */
  def execute[A](effect: PreparedStatement => A)(using c: Connection): A = 
    trace(toString)
    val st = c.prepareStatement(parts.mkString("?"))
    try
      for (p, i) <- params.zipWithIndex
      do p.sqlType.inject(st, i+1, p.value)
      effect(st)
    finally
      st.close

  /**
   * Execute the Query as an update or data definition and return the update count.
   */
  def update: QueryResult[Int] = 
    new:
      def fold[S](s0: S)(f: (S, Int) => S)(using c: Connection): S =
        f(s0, execute(_.executeUpdate()))

  def results: QueryResult[Row] = 
    new:
      def fold[S](s0: S)(f: (S, Row) => S)(using c: Connection): S =
        execute {
          (st) =>
            val rs = st.executeQuery()
            var s = s0 
            for cursor <- Option(rs) 
            do
              try
                while cursor.next() 
                do s = f(s, Row(cursor))
              finally
                cursor.close
            s
        }

object Query:
  /**
   * Construct a static Query fragment explicitly.
   */
  def apply(text: String): Query =
    new:
      val parts = Seq(text)
      val params = Seq()

  /**
   * Construct a Query fragment consisting of a parameter.
   */
  def apply(param: Param): Query =
    new:
      val parts = Seq("", "")
      val params = Seq(param)

  /**
   * Construct comma separated parameters.
   */
  def apply(ps: Seq[Param]): Query =
    new:
      val parts = 
        ps.size match
          case 0 => Seq("")
          case 1 => Seq("", "")
          case n => Seq("") ++ Seq.fill(n-1)(", ") ++ Seq("")
      val params = ps
   
/**
 * The `sql` string interpolator.
 */
extension( sc: StringContext)
  def sql( ps: Param* ): Query = 
    new:
      val parts = sc.parts
      val params = ps

trait QueryResult[+R]:
  outer =>
    def fold[S](s0: S)(f: (S, R) => S)(using Connection): S
    def map[T](g: R => T): QueryResult[T] =
      new:
        def fold[S](s0: S)(f: (S, T) => S)(using Connection): S = outer.fold(s0)((s, r) => f(s, g(r)))
    def ++[T >: R](other: QueryResult[T]): QueryResult[T] =
      new:
        def fold[S](s0: S)(f: (S, T) => S)(using Connection): S =
          val s1 =outer.fold(s0)(f)
          other.fold(s1)(f)
    def foreach(loop: R => Unit)(using Connection): Unit = 
      fold(())((_, r) => loop(r))
    def withFilter(pred: R => Boolean): QueryResult[R] =
      new:
        def fold[S](s0: S)(f: (S, R) => S)(using Connection): S =
          outer.fold(s0)((s, r) => if pred(r) then f(s, r) else s)
    def apply()(using c: Connection, e: R <:< Int): Int =
      fold(0)((s, n) => s + e(n))

object QueryResult:
  val empty: QueryResult[Nothing] =
    new:
      def fold[S](s0: S)(f: (S, Nothing) => S)(using Connection): S = s0
      
  def concat[R](qrs: Iterable[QueryResult[R]]): QueryResult[R] =
    if qrs.nonEmpty then qrs.reduce(_ ++ _)
    else empty

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

