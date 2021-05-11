package summit

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

inline val tracing = false
inline def trace(t: => String): Unit =
  inline if tracing then println(t)

/**
 * Describes an element (a member) of a product type such as a case class.
 * The product type is `B` and element is type `A`, which is abstract here.
 * 
 * Although abstract, `A` is described by `typeclass`, an instance of `T[A]`.
 * Also, a value of the element can be obtained from a `B` by `pick`. 
 * 
 * An element has a `label`, which is its member name within the product `B`.
 * It has an index which is its position in the product counting from 0.
 */ 
trait Element[B, T[_]]:
  type A 
  def label: String
  def index: Int
  def pick: B => A
  def typeclass: T[A]
  
/**
 * Describes a type that is both a product and the member of a sum,
 * typically a case class.
 * 
 * The sum type is `C` and the product type is `B`, which is abstract here.
 * 
 * Although abstract, `B` is described by its `elements`. The element types are
 * constrained to typeclass `T`.  That is, there must be a given `T[A]` for 
 * an element of type `A`.  Otherwise the compiler reports an error.
 * 
 * A value of `B` and can be safely `upcast` to `C`. 
 * 
 * The product has a `label` which is its unqualified type name. 
 * It has an `ordinal` which is its position counting from 0 in the sum `C`.  
 *  
 * A `constructor` creates an instance of `B` from an untyped `Product`. 
 * This operation is not type safe and may raise `ClassCastException` at runtime.
 */ 
trait Branch[C, T[_]]:
  type B
  def label: String
  def ordinal: Int
  def elements: IndexedSeq[Element[B, T]]
  def constructor: Product => B
  def upcast(b: B): C = b.asInstanceOf[C]

 /**
  * Pairs a description of a product and a value of that type.
  * The type of the product, `B` is abstract here. 
  * 
  * The product is a member of a sum type `C` and 
  * its elements have typeclass `T`.
  */  
trait BranchValue[C, T[_]]:
  val branch: Branch[C, T]
  def value: branch.B

/**
 * Describes a sum type, `C`, which is a sealed trait or class or an enum.
 * 
 * The sum is described by its `branches`. Each branch type is a product
 * of element types that are constrained to the typeclass `T`.
 * 
 * The sum type also has a `label` which is its unqualified type name.
 * 
 * An value of `C` can be safely `cast` to a `BranchValue[C, T]`. The
 * specific type of the branch remains abstract.
 * 
 * Similarly, the branch of a value of `C` can be found by `ordinal`
 * which returns its position counting from 0.
 */ 
trait SumType[C, T[_]] extends SumOrProductType[C, T]:
  def label: String
  def branches: IndexedSeq[Branch[C, T]]
  def ordinal(c: C): Int
  def cast(c: C): BranchValue[C, T] =
    new:
      val branch = branches(ordinal(c))
      val value = c.asInstanceOf[branch.B]

object SumType:
  /**
   * Construct a sum description from the compiler's Mirror type.
   */   
  inline def apply[C, T[_]](using m: Mirror.SumOf[C]): SumType[C, T] =
    trace(s"constructing SumType[${Helpers.label}]")
    new:
      val label = Helpers.label
      val branches = Helpers.branches
      def ordinal(c: C) = Helpers.ordinal(c) 

  /**
   * A given for the sum description, for convenience in deriving typeclasses.
   */ 
  inline given sumType[C, T[_]](using m: Mirror.SumOf[C]): SumType[C, T] = apply

/**
 * Describes a product type such as a case class or a tuple. 
 * 
 * The product type is `P`. It is described by its `elements`. 
 * 
 * The element types are constrained to typeclass `T`.  That is, 
 * there must be a given `T[A]` for an element of type `A`.  
 * Otherwise the compiler reports an error.
 * 
 * The product has a `label` which is its unqualified type name. 
 *  
 * A `constructor` creates an instance of `B` from an untyped `Product`. 
 * This operation is not type safe and may raise `ClassCastException` at runtime.
 */ 
trait ProductType[P, T[_]] extends SumOrProductType[P, T]:
  def label: String
  def elements: IndexedSeq[Element[P, T]]
  def constructor: Product => P

object ProductType:

  /**
   * Extract a product description from a branch description.
   */ 
  def apply[C, T[_]](branch: Branch[C, T]): ProductType[branch.B, T] =
    new:
      val label = branch.label
      val elements = branch.elements
      val constructor = branch.constructor

  /**
   * Construct a product description from the compiler's Mirror type.
   */ 
  inline def apply[P, T[_]](using m: Mirror.ProductOf[P]): ProductType[P, T] =
    trace(s"constructing ProductType[${Helpers.label}]")
    new:
      val label = Helpers.label
      val elements = Helpers.elements
      val constructor = Helpers.constructor

  /**
   * A given for the product description, for convenience in deriving typeclasses.
   */ 
  inline given productType[B, T[_]](using Mirror.ProductOf[B]): ProductType[B, T] = apply

/**
 * The sum of both description types.  This is needed when a typeclass must
 * be derived for both an enum and a case class.
 */ 
sealed trait SumOrProductType[R, T[_]]:
  def label: String

object SumOrProductType:
  /**
   * Construct a sum or product description from the compiler's Mirror type.
   */ 
  inline def apply[R, T[_]](using m: Mirror.Of[R]): SumOrProductType[R, T] =
    inline m match
      case given Mirror.SumOf[R] => SumType[R, T]
      case given Mirror.ProductOf[R] => ProductType[R, T]

  /**
   * A given for the sum or product description, for convenience in deriving typeclasses.
   */ 
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
