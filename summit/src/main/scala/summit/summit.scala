package summit

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

inline val tracing = false
inline def trace(t: => String): Unit =
  inline if tracing then println(t)

trait Element[B, T[_]]:
  type A 
  def label: String
  def index: Int
  def pick: B => A
  def typeclass: T[A]
  
trait Branch[C, T[_]]:
  type B
  def label: String
  def ordinal: Int
  def elements: IndexedSeq[Element[B, T]]
  def constructor: Product => B
  def upcast(b: B): C = b.asInstanceOf[C]

trait BranchValue[C, T[_]]:
  val branch: Branch[C, T]
  def value: branch.B

trait SumType[C, T[_]] extends SumOrProductType[C, T]:
  def label: String
  def branches: IndexedSeq[Branch[C, T]]
  def ordinal(c: C): Int
  inline def cast(c: C): BranchValue[C, T] =
    new:
      val branch = branches(ordinal(c))
      val value = c.asInstanceOf[branch.B]

object SumType:
  inline def apply[C, T[_]](using m: Mirror.SumOf[C]): SumType[C, T] =
    trace(s"constructing SumType[${Helpers.label}]")
    new:
      val label = Helpers.label
      val branches = Helpers.branches
      def ordinal(c: C) = Helpers.ordinal(c) 

  inline given sumType[C, T[_]](using m: Mirror.SumOf[C]): SumType[C, T] = apply

trait ProductType[P, T[_]] extends SumOrProductType[P, T]:
  def label: String
  def elements: IndexedSeq[Element[P, T]]
  def constructor: Product => P

object ProductType:
  def apply[C, T[_]](branch: Branch[C, T]): ProductType[branch.B, T] =
    new:
      val label = branch.label
      val elements = branch.elements
      val constructor = branch.constructor

  inline def apply[P, T[_]](using m: Mirror.ProductOf[P]): ProductType[P, T] =
    trace(s"constructing ProductType[${Helpers.label}]")
    new:
      val label = Helpers.label
      val elements = Helpers.elements
      val constructor = Helpers.constructor

  inline given productType[B, T[_]](using Mirror.ProductOf[B]): ProductType[B, T] = apply

sealed trait SumOrProductType[R, T[_]]:
  def label: String

object SumOrProductType:
  inline def apply[R, T[_]](using m: Mirror.Of[R]): SumOrProductType[R, T] =
    inline m match
      case given Mirror.SumOf[R] => SumType[R, T]
      case given Mirror.ProductOf[R] => ProductType[R, T]

  inline given sumOrProductType[B, T[_]](using Mirror.Of[B]): SumOrProductType[B, T] = apply


object Helpers:

  inline def label[C](using m: Mirror.Of[C]): String = constValue[m.MirroredLabel]

  inline def ordinal[C](c: C)(using m: Mirror.SumOf[C]): Int = m.ordinal(c)

  inline def labels[C](using m: Mirror.Of[C]): List[String] = expandLabels[m.MirroredElemLabels]

  inline def expandLabels[E <: Tuple]: List[String] =
    inline erasedValue[E] match
      case _: EmptyTuple => Nil
      case _: (e *: es) => constValue[e].toString :: expandLabels[es]

  inline def elements[B, T[_]](using m: Mirror.ProductOf[B]): IndexedSeq[Element[B, T]] = 
    expandElements[B, m.MirroredElemTypes, T](labels[B], 0).toIndexedSeq

  inline def expandElements[B, E <: Tuple, T[_]](ls: List[String], ix: Int): List[Element[B, T]] =
    inline erasedValue[E] match
      case _: EmptyTuple => Nil
      case _: (e *: es) => 
        val element =
          new Element[B, T]:
            type A = e
            val label = ls.head
            val index = ix
            val pick = _.asInstanceOf[Product].productElement(ix).asInstanceOf[A]
            val typeclass = summonInline[T[e]]
        element :: expandElements[B, es, T](ls.tail, ix+1)

  inline def constructor[B](using m: Mirror.ProductOf[B]): Product => B = m.fromProduct

  inline def constructorFromSeq[B](using m: Mirror.ProductOf[B]): IndexedSeq[Any] => B =
    es => 
      val p: Product = 
        new:
          def productArity = es.size
          def productElement(i: Int) = es(i)
          def canEqual(a: Any) = false

      m.fromProduct(p)

  inline def branches[C, T[_]](using m: Mirror.SumOf[C]): IndexedSeq[Branch[C, T]] = 
    expandBranches[C, m.MirroredElemTypes, T](labels[C], 0).toIndexedSeq

  inline def expandBranches[C, E <: Tuple, T[_]](ls: List[String], ix: Int): List[Branch[C, T]] =
    inline erasedValue[E] match
      case _: EmptyTuple => Nil
      case _: (e *: es) => 
        val branch =
          new Branch[C, T]:
            type B = e
            val label = ls.head
            val ordinal = ix
            val elements = Helpers.elements(using summonInline[Mirror.ProductOf[e]])
            val constructor = Helpers.constructor(using summonInline[Mirror.ProductOf[e]])
        branch :: expandBranches[C, es, T](ls.tail, ix+1)
end Helpers
