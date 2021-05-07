# summit

Easy-to-use sum and product type descriptions generated from the compiler's `Mirror` types.

## Features

- The description's are values.  No programming in the inline zone is needed to use them. 
- Create typeclass instances as `given` definitions or with `deriving` methods.
- Define a typeclass for sums, products or both without ambiguity. 
- Specify any element typeclass.  It may be the same as your product or sum typeclass or not.
- Deconstruct sums and products with type safety.

# Guide

This guide will cover:

- Products and `ProductType`
- Sums and `SumType`
- Behaviour of `given` vs. `derives`
- Ambiguous `deriving` and `SumOrProductType`

### Products and `ProductType`

Let's implement the `Show` typeclass. It is usefull to pretty-print data structures, 
like a more easily customised `toString`.

```scala
trait Show[A]:
  def show(a: A): String

def showit[A](a: A)(using s: Show[A]): String = s.show(a)

```

This defines a method `showit` that will generate a string for a value of type `A`, 
provided there exists a `given Show[A]`.  Here are some givens:

```scala
object Show:

  given Show[String] with
    def show(s: String) = s

  given Show[Int] with
    def show(i: Int) = i.toString

```

Now try it out:

```scala
showit("age")       // return "age"
showit(49)          // returns "49"
showit(("age", 49)) // error: no implicit argument of type Show[(String, Int)]  ...
```

We can define a `given Show[(String, Int)]` to fix the error for this one case. Or we can use summit to handle any tuple or case class. In other words, any product type.

In the following, `showProduct[P]` creates a `Show[P]` using a `summit.ProductType[P, Show]`. 
This `ProductType[P, Show]` will be given iff:

  - `P` is a product type
  - There is a given instance of `Show` for each element (ie member) of `P`.

 So `(String, Int)` would qualify because we have givens `Show[String]` and `Show[Int]`.  

```scala
object Show:

  ...

  import summit.{ProductType, Element}

  given showProduct[P](using prod: ProductType[P, Show]): Show[P] = // 1
    new Show[P]:
      def show(p: P) = formatProduct(p, prod.label, prod.elements)  // 2

  def formatProduct[P](p: P, l: String, es: IndexedSeq[Element[P, Show]]): String =
    val m = for e <- es yield e.typeclass.show(e.pick(p))           // 3
    val n = m.mkString(", ")
    s"$l($n)"
```

The formula used here is:

  1. Summon an instance of `ProductType[P, Show]` containing information about `P`
     including `Show` instances for its elements.

  2. Create an instance of `Show[P]`.  Here, `label` is the 
     type name of the actual type represented by `P`.  
     And `elements` is a sequence of element descriptions, 
     one for each element of `P`.

  3. To show `p`, first show each element using an `Element[P, Show]`. 
     The `typeclass` is an instance of `Show` for the element 
     and `pick` extracts the element's value from `p`. 

And the rest is string formatting.

