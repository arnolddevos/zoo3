package qeduce

/**
 * Domain models as ADTs with corresponding database schemas.
 */ 
package models

import summit.{SumType, Branch, BranchValue, Element, trace}
import java.sql.{PreparedStatement, Connection}

def insert[R](r: R)(using t: SQLModel[R])(using Connection): Int = t.insert(r)()
def declare[R](using t: SQLModel[R])(using Connection): Int = t.declare()()
def select[R](label: String)(using t: SQLModel[R]): QueryResult[R] = t.select(label)
def selectAll[R](using t: SQLModel[R]): QueryResult[R] = t.selectAll()
def insertAll[R](rs: Iterable[R])(using t: SQLModel[R])(using Connection): Int = t.insertAll(rs)()

trait SQLModel[R]:
  def insert(r: R): QueryResult[Int]
  def declare(): QueryResult[Int]
  def select(label: String): QueryResult[R]
  def selectAll(): QueryResult[R]
  def insertAll(rs: Iterable[R]): QueryResult[Int]

object SQLModel:

  class RowRoduct[R](r: Row, b: Branch[R, SQLType]) extends Product:
    def productElement(ix: Int): Any = 
      val e = b.elements(ix)
      r(e.label)(using e.typeclass)
    def productArity: Int = b.elements.size
    def canEqual(other: Any) = false

  def elementAsParam[B](b: B, e: Element[B, SQLType]): Param =
    Param(e.pick(b))(using e.typeclass)

  def branchAsParams[R](bv: BranchValue[R, SQLType]): Query =
    val ps =
      for e <- bv.branch.elements
      yield elementAsParam(bv.value, e)
    Query(ps)

  def branchAsLabels[R](b: Branch[R, SQLType]): Query =
    val ls =
      for e <- b.elements
      yield e.label
    Query(ls.mkString(", "))

  def derived[R](using sum: SumType[R, SQLType]): SQLModel[R] =
    new:
      trace(s"deriving SQLModel[${sum.label}]")

      def insert(r: R): QueryResult[Int] = 
        val bv = sum.cast(r)
        val qy = 
          sql"insert into" ~ Query(bv.branch.label) ~ 
          sql"("  ~ branchAsLabels(bv.branch) ~ 
          sql") values (" ~ branchAsParams(bv) ~ sql")"
        qy.update

      def declare(): QueryResult[Int] = 
        val qrs = 
          for 
            b <- sum.branches
            qy = sql"create table if not exists" ~ Query(b.label) ~ 
                 sql"(" ~ branchAsLabels(b) ~ sql")"
          yield qy.update

        QueryResult.concat(qrs)

      def select(label: String): QueryResult[R] = 
        val b = sum.branches.find(_.label == label).get
        val q = sql"select" ~ branchAsLabels(b) ~ sql"from" ~ Query(b.label)
        q.results.map(r => b.upcast(b.constructor(RowRoduct(r, b))))

      def selectAll(): QueryResult[R] =
        val qrs =
          for
            b <- sum.branches
            q = sql"select" ~ branchAsLabels(b) ~ sql"from" ~ Query(b.label)
          yield
            q.results.map(r => b.upcast(b.constructor(RowRoduct(r, b))))
        QueryResult.concat(qrs)

      def insertAll(rs: Iterable[R]): QueryResult[Int] = 
        val grps = rs.map(sum.cast(_)).groupBy(_.branch.ordinal)
        val qrs =
          for
            (_, bvs) <- grps
            bv = bvs.head
            qy =
              sql"insert into" ~ Query(bv.branch.label) ~ 
              sql"("  ~ branchAsLabels(bv.branch) ~ 
              sql") values (" ~ branchAsParams(bv) ~ sql")"
          yield
            new QueryResult[Int]:
              def fold[S](s0: S)(f: (S, Int) => S)(using c: Connection): S =
                val n =
                  qy.execute {
                    (st) =>
                      var n = st.executeUpdate()
                      for bv <- bvs.tail
                      do
                        for e <- bv.branch.elements
                        do e.typeclass.inject(st, e.index+1, e.pick(bv.value))
                        n += st.executeUpdate()
                      n
                  }
                f(s0, n)
        QueryResult.concat(qrs)
