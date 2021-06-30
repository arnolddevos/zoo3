package qeduce

/**
 * Domain models as ADTs with corresponding database schemas.
 */ 
package models

import summit.{SumType, ProductType, SumOrProductType, Branch, BranchValue, Element, trace}
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

  def colNames[B](elements: IndexedSeq[Element[B, SQLType]]): Query =
    val ls =
      for e <- elements
      yield e.label
    Query(ls.mkString(", "))

  def colValues[B](b: B, elements: IndexedSeq[Element[B, SQLType]]): Query =
    val ps =
      for e <- elements
      yield Param(e.pick(b))(using e.typeclass)
    Query(ps)

  def insertTemplate[B](b: B, label: String, elements: IndexedSeq[Element[B, SQLType]]): Query =
    sql"insert into" ~ Query(label) ~ 
    sql"("  ~ colNames(elements) ~ 
    sql") values (" ~ colValues(b, elements) ~ sql")"

  def createTemplate[B](label: String, elements: IndexedSeq[Element[B, SQLType]]): Query =
    sql"create table if not exists" ~ Query(label) ~ 
    sql"(" ~ colNames(elements) ~ sql")"

  def selectTemplate[B](label: String, elements: IndexedSeq[Element[B, SQLType]]): Query =
    sql"select" ~ colNames(elements) ~ sql"from" ~ Query(label)

  def repeatedInsert[B](bs: Iterable[B], label: String, elements: IndexedSeq[Element[B, SQLType]]): QueryResult[Int] =
    if bs.isEmpty then QueryResult.empty
    else
      val qy = insertTemplate(bs.head, label, elements)
      new QueryResult[Int]:
        def fold[S](s0: S)(f: (S, Int) => S)(using c: Connection): S =
          val n =
            qy.execute {
              (st) =>
                var n1 = st.executeUpdate()
                for b <- bs.tail
                do
                  for e <- elements
                  do e.typeclass.inject(st, e.index+1, e.pick(b))
                  n1 += st.executeUpdate()
                n1
            }
          f(s0, n)

  def derived[R](using sp: SumOrProductType[R, SQLType]): SQLModel[R] =
    trace(s"deriving SQLModel[${sp.label}]")
    sp match
      case prod: ProductType[R, SQLType] => sqlModelForProduct(prod)
      case sum: SumType[R, SQLType] => sqlModelForSum(sum)

  def sqlModelForProduct[R](prod: ProductType[R, SQLType]) = 
    new SQLModel[R]:
      def insert(r: R): QueryResult[Int] = 
        insertTemplate(r, prod.label, prod.elements).update
      def declare(): QueryResult[Int] = 
        createTemplate(prod.label, prod.elements).update
      def select(label: String): QueryResult[R] = 
        for r <- selectTemplate(label, prod.elements).results
        yield prod.constructor(RowProduct(r, prod.elements))
      def selectAll(): QueryResult[R] = 
        select(prod.label)
      def insertAll(rs: Iterable[R]): QueryResult[Int] = 
        repeatedInsert(rs, prod.label, prod.elements)

  def sqlModelForSum[R](sum: SumType[R, SQLType]) = 
    new SQLModel[R]:
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
        q.results.map(r => b.upcast(b.constructor(RowProduct(r, b.elements))))

      def selectAll(): QueryResult[R] =
        val qrs =
          for
            b <- sum.branches
            q = sql"select" ~ branchAsLabels(b) ~ sql"from" ~ Query(b.label)
          yield
            q.results.map(r => b.upcast(b.constructor(RowProduct(r, b.elements))))
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

class RowProduct[B](row: Row, elements: IndexedSeq[Element[B, SQLType]]) extends Product:
  def productElement(ix: Int): Any = 
    val e = elements(ix)
    row(e.label)(using e.typeclass)
  def productArity: Int = elements.size
  def canEqual(other: Any) = false

extension(rs: QueryResult[Row])
  def as[P](using t: ProductType[P, SQLType]): QueryResult[P] = 
    rs.map(r => t.constructor(RowProduct(r, t.elements)))
