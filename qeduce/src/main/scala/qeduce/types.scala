package qeduce

import java.sql.{ResultSet, PreparedStatement}

/**
 * Instances of SQLType[A] witness that values of A can be stored in the database.
 */
abstract class SQLType[A]:
  def inject: (PreparedStatement, Int, A) => Unit
  def extract: (ResultSet, String) => A
  def display: A => String

object SQLType:
  given SQLType[Int] with
    def extract = _.getInt(_)
    def inject = _.setInt(_, _)
    def display = _.toString

  given SQLType[Long] with
    def extract = _.getLong(_)
    def inject = _.setLong(_, _)
    def display = _.toString

  given SQLType[Double] with
    def extract = _.getDouble(_)
    def inject = _.setDouble(_, _)
    def display = _.toString

  given SQLType[Boolean] with
    def extract = _.getBoolean(_)
    def inject = _.setBoolean(_, _)
    def display = _.toString

  given SQLType[String] with
    def extract = 
      (rs, n) =>
        val s = rs.getString(n)
        if s == null then "" else s

    def inject = _.setString(_, _)
    def display = t => "'" + t.replace("'", "''") + "'"

  given [A]( using u: SQLType[A]): SQLType[Option[A]] with
    def extract =
      (rs, name) =>
        val a = u.extract(rs, name)
        if(rs.wasNull) None else Some(a)
    
    def inject =
      (st, ix, as) =>
        as match
          case Some(a) => u.inject(st, ix, a)
          case None => st.setObject(ix, null)
    
    def display =
      _ match
        case Some(a) => "Some(" + u.display(a) + ")"
        case None => "None"
