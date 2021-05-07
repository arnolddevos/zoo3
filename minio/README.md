# minIO 

A simple concurrent effects system.  Highlights:

* IO monad with typed error channel.
* Fibers, racing and interruption.
* Transactor for non-blocking concurrent data structures.
* Familiar usage, similar to basic ZIO.
* Compact implementation in scala 3.

## Why?

* As an update to (the author's) flowlib.
* An IO for minimalists.
* As a tutorial (because the code is compact).
* As a test bed for a scala 3 effects framework.

## Usage

```scala
import minio.api._
```

The principal type `IO[+E, +A]` represents an effect that may eventually succeed with a value of `A` or fail with a value of `E`. Constructors for `IO` values include `effect` and `effectAsync` and combinators include `flatMap`, `zip` and `race`. See _Signature.scala_.

The main supporting types are `Fiber` and `Transactor`.  

A `Fiber[+E, +A]` represents a lightweight thread runnining an effect. Fiber operations include, `fork`, `join`, `await` and `interrupt`.  See _Fibers.scala_.

A `Transactor[S]` manages a variable of type `S`, the _state_.  It orders and sequentially executes `Transaction`s which are functions on state. Basic transactors include `semaphore`, `barrier` and `queue`. See _Synchronization.scala_.

## Architecture

The API is defined in a trait, `Signature`. The idea is that alternative implementations can be tried.  

This current implementation is seen in _Simple.scala_. It defines `IO` as an `enum` with a small number of cases underlying a richer set of combinators and contructors.   

The interpreter `runFiber` is likewise simple.  No trampoline is used.  Instead, the depth of the stack is tracked and the execution is shifted to a new thread if a limit is exceeded. 

The implementation is supported by modules `Fibers` and `Synchronization`.  

## Fibers

This module defines `Fiber`, `Arbiter` and `Runtime`.

A `Runtime` provides methods to run effects: `unsafeRunAsync` and `unsafeRunSync`.  It depends on a `Platform` which, in this version, encapsulates a java `ForkJoinPool`. 

A `Fiber` represents an effect in progress. Top level fibers are created by a `Runtime`. Child fibers are subsequently created by `fork`. 

The semantics are intended to be the same as ZIO.

Class `Arbiter` is not part of the API. An arbiter manages a group of fibers and provides the `race` operation.  

## Synchronization

This module defines `Transactor` and `Gate`. 

A `Transactor[State]` holds the state of each `Fiber`, `Arbiter` and `Gate`. Operations such as `fork`, `join` and `interrupt` are transactions.

A transaction is modeled as a pure function on state which may return a new state and a result effect. Or it may return the value `Blocked`.  Blocked transactions are retained in the transactor until they can produce an effect.

A transactor provides `transact[E, A](tx: Transaction[State, IO[E, A]]): IO[E, A]`.  This lifts a transaction into an effect that will execute a state change and return a result `A` or error `E`. 

A `Gate[-A, +B]` is a higher level data structure based on `Transactor` that concurrently accepts values of type `A` and provides values of type `B`. These operations are called `offer` and `take` rsp.  Gates provided include `semaphore`, `queue`, and `barrier`.
