package qeduce

import java.sql.{ResultSet, PreparedStatement}

/**
 * Instances of SQLType[A] witness that values of A can be stored in the database.
 */
abstract class SQLType[A]: 
  parent =>
  def inject: (PreparedStatement, Int, A) => Unit
  def extract: (ResultSet, String) => A
  def display: A => String
  def parse: String => Option[A]
  def map[B](f: A => B)(g: B=> A): SQLType[B] =
    new:
      val inject = (s, i, b) => parent.inject(s, i, g(b))
      val extract = (rs, n) => f(parent.extract(rs, n))
      val display = b => parent.display(g(b))
      val parse = t => parent.parse(t).map(f)

object SQLType:
  given SQLType[Int] with
    val extract = _.getInt(_)
    val inject = _.setInt(_, _)
    val display = _.toString
    val parse = _.toIntOption

  given SQLType[Long] with
    val extract = _.getLong(_)
    val inject = _.setLong(_, _)
    val display = _.toString
    val parse = _.toLongOption

  given SQLType[Double] with
    val extract = _.getDouble(_)
    val inject = _.setDouble(_, _)
    val display = _.toString
    val parse = _.toDoubleOption

  given SQLType[Boolean] with
    val extract = _.getBoolean(_)
    val inject = _.setBoolean(_, _)
    val display = _.toString
    val parse = _.toBooleanOption

  given SQLType[String] with
    val extract = 
      (rs, n) =>
        val s = rs.getString(n)
        if s == null then "" else s

    val inject = _.setString(_, _)
    val display = t => "'" + t.replace("'", "''") + "'"
    val parse = Option(_)

  given [A]( using u: SQLType[A]): SQLType[Option[A]] with
    val extract =
      (rs, name) =>
        val a = u.extract(rs, name)
        if(rs.wasNull) None else Some(a)
    
    val inject =
      (st, ix, as) =>
        as match
          case Some(a) => u.inject(st, ix, a)
          case None => st.setObject(ix, null)
    
    val display =
      _ match
        case Some(a) => "Some(" + u.display(a) + ")"
        case None => "None"

    val parse =
      t => u.parse(t).map(Option(_))
