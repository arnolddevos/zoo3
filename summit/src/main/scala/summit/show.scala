package summit
package show

trait Show[A]:
  def show(a: A): String

def showit[R](r: R)(using s: Show[R]): String = s.show(r)

object Show:

  given Show[String] with
    def show(s: String) = s

  given Show[Int] with
    def show(i: Int) = i.toString

  given Show[Double] with
    def show(d: Double) = d.toString

  given Show[Long] with
    def show(d: Long) = d.toString

  given Show[BigDecimal] with
    def show(d: BigDecimal) = d.toString

  given Show[BigInt] with
    def show(d: BigInt) = d.toString

  given Show[Float] with
    def show(d: Float) = d.toString

  given Show[Short] with
    def show(d: Short) = d.toString

  given showProduct[P](using prod: ProductType[P, Show]): Show[P] =
    trace(s"showProduct ${prod.label}")
    new Show[P]:
      def show(p: P): String =
        formatProduct(p, prod.label, prod.elements)

  given showSum[C](using sum: SumType[C, Show]): Show[C] = 
    trace(s"showSum ${sum.label}")
    new Show[C]:
      def show(c: C): String =
        val bv = sum.cast(c)
        formatProduct(bv.value, bv.branch.label, bv.branch.elements)

  def formatProduct[P](p: P, l: String, es: IndexedSeq[Element[P, Show]]): String =
    val m = for e <- es yield e.typeclass.show(e.pick(p))
    val n = m.mkString(", ")
    s"$l($n)"


  def derived[R](using sp: SumOrProductType[R, Show]): Show[R] =
    trace(s"deriving Show[${sp.label}]")
    sp match
      case given ProductType[R, Show] => showProduct[R]
      case given SumType[R, Show] => showSum[R]

