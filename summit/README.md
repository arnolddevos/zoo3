# summit

Easy-to-use sum and product type descriptions generated from the compiler's `Mirror` types.

## Features

- The description's are values.  No programming in the inline zone is needed to use them. 
- Create your typeclass instances as `given` definitions or via `derived` methods.
- Define a your typeclass for sums, products or both without ambiguity. 
- Specify different typeclasses for a product and its the elements.
- Deconstruct generic sums and products with type safety.

See below for:

- The Types Provided 
- Using `derives` versus `given`
- A worked example

# The Types Provided

## `ProductType`

Describes a product type such as a case class or a tuple. 

### Usage

To define an instance of a typeclass, `TC`, for a type `P` using a `ProductType` place one of these definitions in the companion object of `TC`:

```scala
given[P](using info: ProductType[P, T]): TC[P] = ...
```
or
```scala
def derived[P](using info: ProductType[P, T]): TC[P] = ...
```

Here `T` is a typeclass for the elements of `P`.  In many examples `T` will be the same as `TC`.   The rule for T is: 

> If `A` is the type of an element of `P` there must be a given `T[A]`.

The `ProductType` is available if this rule holds and if `P` is actually a product type.  Otherwise the compiler reports an error.

### Definitions

```scala
trait ProductType[P, T[_]] extends SumOrProductType[P, T]:
  def label: String
  def elements: IndexedSeq[Element[P, T]]
  def constructor: Product => P
```

- `P` is a product type.
- `T` is a typeclass for the elements of `P`.
- `label` is the unqualified type name of the actual type represented by `P`.
- `elements` contains a description of each element of `P`, in the order they are defined.
- `constructor` is a function that produces a new `P` from an untyped product.  This will raise a runtime exception if the element types do not match or the arities don't agree.

An `Element` describes an individual element (a member) of `P`.

```scala
trait Element[P, T[_]]:
  type A 
  def label: String
  def index: Int
  def pick: B => A
  def typeclass: T[A]
```
- `A` is the element type.
- `label` is this element's member name within the product.
- `index` is the position of this element in definition order counting from 0.
- `typeclass` is a typeclass instance for this element.
- `pick` is a function returning the element value from a product.

## `SumType`

Describes a sum type such as a sealed trait or an enum. A sum is composed of products, called branches here, which are in turn composed of elements.

### Usage

To define an instance of a typeclass, `TC`, for a type `C` using a `SumType` place one of these definitions in the companion object of `TC`:

```scala
given[C](using info: SumType[C, T]): TC[C] = ...
```
or
```scala
def derived[C](using info: SumType[C, T]): TC[C] = ...
```

Here `T` is a typeclass for the elements of the branches of `C`.  In many examples `T` will be the same as `TC`.   The rule for T is: 

> If `A` is the type of an element of a branch of `C` there must be a given `T[A]`.

### Definitions

```scala
trait SumType[C, T[_]] extends SumOrProductType[C, T]:
  def label: String
  def branches: IndexedSeq[Branch[C, T]]
  def ordinal(c: C): Int
  def cast(c: C): BranchValue[C, T]
```

- `C` is a sum type.
- `T` is a typeclass for the elements of the branches of `C`.
- `label` is the unqualified type name of the actual type represented by `C`.
- `branches` contains a description of each branch of `C`, in the order they are defined.
- `cast` safely down-casts a value of `C` to a value of a branch type.  The branch type is abstract in this situation. The branch value is paired with a branch description.

```scala
trait Branch[C, T[_]]:
  type B
  def label: String
  def ordinal: Int
  def elements: IndexedSeq[Element[B, T]]
  def constructor: Product => B
  def upcast(b: B): C 
```

- `B` is the type of a branch of `C`.
- `ordinal` is the position of this branch in definition order counting from 0.
- `upcast` casts a value of `B` to `C`. The upcast must be explicit because `B` is abstract in this context.

The `label`, `elements` and `constructor` members of `Branch` are as for `ProductType`.  A `ProductType` can be constructed from a `Branch`:

```scala
  object ProductType:
    def apply[C, T[_]](branch: Branch[C, T]): ProductType[branch.B, T]
```

```scala
trait BranchValue[C, T[_]]:
  val branch: Branch[C, T]
  def value: branch.B
```

- `branch` is a branch of `C`.
- `value` is a value of the branch type.

# Using `derives` versus `given`

## `given`

A `given` definition is the obvious way to create instances for a typeclass, `TC`.  

```scala
given[P](using ProductType[P, T]): TC[P] = ...
given[C](using SumType[C, T]): TC[C] = ...
```

- Definitions can be provided for both sum and product types and these do not clash.

- The definition can be placed anywhere in the _implicit scope_ (although it would usually be in the companion of `TC`).  

- The given will be available for any `P` or `C` if the `SumType` or `ProductType` resp. can be constructed.

However, a disadvantage is that an instance of `TC` and its underlying `SumType` or `ProductType` will be constructed each time it is used.  That might be a significant cost.

## `derives`

A `derived` definition for `TC` is the alternative.  

```scala
def derived[C](using SumType[C, T]): TC[C] = ...
def derived[P](using ProductType[P, T]): TC[P] = ...
def derived[R](using SumOrProductType[R, T]): TC[R] = ...
case class Person(name: String, age: Int) derives TC
```
- The `derived` method must be defined in the companion of the typeclass and the `derives` clause added to the definition of each applicable type, such as `Person` above.

- An instance of `TC` will be created at most once for each applicable type.  For example, `TC[Person]` will be constructed at most once.

- Overloading the `derived` method creates a conflict. Only one of the defintions above may be present. Use `SumOrProductType` if the the `TC` applies to both sums and products:

```scala
def derived[R](using sp: SumOrProductType[R, T]): TC[R] =
  sp match
    case given ProductType[R, T] => ...
    case given SumType[R, T] => ...
```

## Both

It is possible to provide both a `given` and a `derived` definition for the same typeclass, `TC`.  

```scala
given tcP[P](using ProductType[P, T]): TC[P] = ...
given tcC[C](using SumType[C, T]): TC[C] = ...
def derived[R](using sp: SumOrProductType[R, T]): TC[R] =
  sp match
    case given ProductType[R, T] => tcP[R]
    case given SumType[R, T]     => tcC[R]

```

In this situation, the `derived` definition is used for the applicable type, 
such as `Person`, iff it has a `derives TC` clause. 
Otherwise, the `given` definition is used and a new instance of `TC`
is constructed for each use.

# A Worked Example

Let's implement the `Show` typeclass. It is usefull to pretty-print 
data structures. Like a more easily customised `toString`.

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

We can define a given `Show[(String, Int)]` to implement this last case.

Or, more generally, we could define a given `Show[(A, B)]` where A and B are 
any types with given `Show[A]` and `Show[B]` resp.

Or, even more generally, we can define a given `Show[P]` 
where `P` is any tuple or case class
that has a given `Show` instance for each element. 

```scala
object Show:
  import summit.{ProductType, Element}

  given[P](using ptype: ProductType[P, Show]): Show[P] with        // 1
    def show(p: P) = formatProduct(p, ptype.label, ptype.elements) // 2

  def formatProduct[P](p: P, l: String, es: IndexedSeq[Element[P, Show]]): String =
    val ns = for e <- es yield e.typeclass.show(e.pick(p))        // 3
    val n = ns.mkString(", ")
    s"$l($n)"

  ...
```

The formula used here is:

  1. Summon an instance of `ProductType[P, Show]` containing information about `P`
     including `Show` instances for its elements.

  2. Create an instance of `Show[P]`.  Here, `label` is the 
     name of the actual type represented by `P`.  

     And `elements` is a sequence of element descriptions, 
     one for each element of `P`.

  3. To show `p`, first show each element using an `Element[P, Show]`. 

     The `typeclass` is an instance of `Show` for the element 
     and `pick` extracts the element's value from `p`. 

And the rest is string formatting.


